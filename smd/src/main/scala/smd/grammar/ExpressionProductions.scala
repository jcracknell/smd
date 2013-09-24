package smd
package grammar

import smd.parsing.{GraphemeParser, LiteralSetParser, OrderedChoiceParser}

trait ExpressionProductions extends CommonProductions {
  def expr: Parser[Expression] = conditionalExpression

  lazy val conditionalExpression: Parser[Expression] = (
    logicalOrExpression
  ~ ( expressionWhitespace_? ~ "?" ~ expressionWhitespace_?
    ~ <>(conditionalExpression)
    ~ expressionWhitespace_? ~ ":" ~ expressionWhitespace_?
    ~ <>(conditionalExpression)
    ^*{ p => (p._4, p._8) }
    ).? ^* {
      case (i, Some((t, e))) => expression.Conditional(i, t, e)
      case (e, None) => e
    }
  )

  lazy val logicalOrExpression = binOp(logicalAndExpression,  "||" ~ !:("=")       ^^^ expression.LogicalOr)

  lazy val logicalAndExpression = binOp(bitwiseOrExpression,  "&&" ~ !:("=")       ^^^ expression.LogicalAnd)

  lazy val bitwiseOrExpression = binOp(bitwiseXOrExpression,  "|"  ~ !:("|" | "=") ^^^ expression.BitwiseOr)

  lazy val bitwiseXOrExpression = binOp(bitwiseAndExpression, "^"  ~ !:("=")       ^^^ expression.BitwiseXOr)

  lazy val bitwiseAndExpression = binOp(equalityExpression,   "&"  ~ !:("&" | "=") ^^^ expression.BitwiseAnd)

  lazy val equalityExpression = binOp(relationalExpression,
    "===" ^^^ expression.StrictEquals
  | "=="  ^^^ expression.Equals
  | "!==" ^^^ expression.StrictNotEquals
  | "!="  ^^^ expression.NotEquals
  )

  lazy val relationalExpression: Parser[Expression] = binOp(shiftExpression,
    ">="         ^^^ expression.GreaterThanOrEqualTo
  | "<="         ^^^ expression.LessThanOrEqualTo
  | ">"          ^^^ expression.GreaterThan
  | "<"          ^^^ expression.LessThan
  | "instanceof" ^^^ expression.InstanceOf
  | "in"         ^^^ expression.In
  )

  lazy val shiftExpression: Parser[Expression] = binOp(additiveExpression,
    "<<"  ^^^ expression.LeftShift
  | ">>>" ^^^ expression.UnsignedRightShift
  | ">>"  ^^^ expression.RightShift
  )

  lazy val additiveExpression: Parser[Expression] = binOp(multiplicativeExpression,
    "+" ^^^ expression.Addition
  | "-" ^^^ expression.Subtraction
  )

  lazy val multiplicativeExpression: Parser[Expression] = binOp(unaryExpression,
    "*" ^^^ expression.Multiplication
  | "/" ^^^ expression.Division
  | "%" ^^^ expression.Modulo
  )

  private def binOp[A](operand: Parser[A], ops: Parser[(A, A) => A]): Parser[A] =
    operand ~ (
      expressionWhitespace_? ~ ops ~ expressionWhitespace_? ~ operand ^* { case (_, op, _, rhs) => (op, rhs) }
    ).* ^* { case (lhs, ops) => (lhs /: ops) { (body, op) => op._1(body, op._2) } }

  lazy val unaryExpression: Parser[Expression] = (
    unaryOp("!",                                     expression.LogicalNot)
  | unaryOp("--",                                    expression.PrefixDecrement)
  | unaryOp("-",                                     expression.Negative)
  | unaryOp("++",                                    expression.PrefixIncrement)
  | unaryOp("+",                                     expression.Positive)
  | unaryOp("~",                                     expression.BitwiseNot)
  | unaryOp("typeof" ~ !:(identifierExpressionPart), expression.TypeOf)
  | unaryOp("delete" ~ !:(identifierExpressionPart), expression.Delete)
  | unaryOp("void"   ~ !:(identifierExpressionPart), expression.Void)
  | postfixExpression
  )

  private def unaryOp(op: Parser[Any], builder: Expression => Expression): Parser[Expression] =
    op ~ expressionWhitespace_? ~> <>(unaryExpression) ^*(builder(_))

  lazy val postfixExpression: Parser[Expression] =
    leftHandSideExpression ~ ( expressionWhitespaceNoNewline_? ~>
      "--" ^^^ expression.PostfixDecrement |
      "++" ^^^ expression.PostfixIncrement
    ).? ^* {
      case (body, Some(builder)) => builder(body)
      case (body, _) => body
    }

  lazy val leftHandSideExpression: Parser[Expression] = {
    val staticProperty =  "." ~ expressionWhitespace_? ~> identifier
    val dynamicProperty = "[" ~ expressionWhitespace_? ~> <>(expr) <~ expressionWhitespace_? ~ "]"

    atExpression ~ (expressionWhitespace_? ~ (
      // Build a sequence of functions which will construct the appropriate expr when provided a body
      argumentList    ^* { args => (b: Expression) => expression.Call(b, args) } |
      staticProperty  ^* { prop => (b: Expression) => expression.StaticProperty(b, prop) } |
      dynamicProperty ^* { prop => (b: Expression) => expression.DynamicProperty(b, prop) }
    )).* ^* { p =>
      val body = p._1
      val builders: Seq[Expression => Expression] = p._2.map(_._2)
      (body /: builders) { (x, b) => b(x) }
    }
  }

  lazy val atExpression: Parser[Expression] = atExpressionRequired | primaryExpression
  lazy val atExpressionRequired: Parser[Expression] = "@" ~ (identifierExpression | primaryExpression) ^*(_._2)

  /** A literal, array literal, or object literal expression. */
  lazy val primaryExpression: Parser[Expression] = (
    literalExpression
  | arrayLiteralExpression
  | objectLiteralExpression
  | "(" ~ expressionWhitespace_? ~> <>(expr) <~ expressionWhitespace_? ~ ")"
  )

  lazy val arrayLiteralExpression: Parser[expression.ArrayLiteral] = {
    /** A non-elided array element preceded by any number of elided elements. */
    val subsequentArrayElement =
      argumentSeparator.+ ~ expressionWhitespace_? ~ <>(expr) ^* { case (seps, _, e) => seps.tail.map(_ => expression.Elided()) :+ e }

    val arrayElements = (
      (
        <>(expr) ~ subsequentArrayElement.* ^* { case (e, ses) => e +: ses.flatten }
      | subsequentArrayElement.+            ^* { p => expression.Elided() +: p.flatten } // initial element elided
      ).?
    <~ argumentSeparator.*
    ^*(_.getOrElse(Seq()))
    )

    "[" ~ expressionWhitespace_? ~> arrayElements <~ expressionWhitespace_? ~ "]" ^* expression.ArrayLiteral
  }

  lazy val objectLiteralExpression: Parser[expression.ObjectLiteral] = {
    val propertyName: Parser[String] = (
      stringLiteralExpression ^*(_.value)
    | verbatimLiteralExpression ^*(_.value)
    | iriLiteralExpression ^*(_.value)
    | numericLiteralExpression ^*(_.value.toString)
    | identifier
    )

    val objectPropertyAssignment =
      propertyName ~ expressionWhitespace_? ~ ":" ~ expressionWhitespace_? ~ <>(expr) ^* { p => (p._1, p._5) }

    val objectPropertyAssignments = (
      objectPropertyAssignment
    ~ (argumentSeparator ~ objectPropertyAssignment).*
    ~ argumentSeparator.?
    ^* { p => p._1 +: p._2.map(_._2) }
    )

    (
      "{" ~ expressionWhitespace_? ~> objectPropertyAssignments.? <~ expressionWhitespace_? ~ "}"
    ^*{ ps => expression.ObjectLiteral(ps.getOrElse(Seq())) }
    )
  }

  // Identifiers

  lazy val identifierExpression: Parser[expression.Identifier] = identifier ^* expression.Identifier

  protected lazy val identifier: Parser[String] =
    // Not a keyword: not a keyword followed by something other than an identifier part
    !:(keyword ~ !:(identifierExpressionPart)) ~>
    identifierExpressionStart ~ identifierExpressionPart.* ^* { case (s, ps) => (new StringBuilder(s) /: ps.flatten) { (sb, p) => sb.append(p) }.toString }

  /** A valid non-initial portion of an identifier. */
  protected lazy val identifierExpressionPart: Parser[String] = identPartCriteria ^* (_.charSequence.toString) | identPartEscape

  private lazy val identifierExpressionStart: Parser[String] = identStartCriteria ^* (_.charSequence.toString) | identStartEscape

  // Escape sequences are allowed in identifiers on the condition that the characters they introduce are valid
  // in the event that they are substituted for the escapes; thus here we filter escapes using the code point
  // criteria established for unescaped characters.

  /** An escape sequence which can occur after the initial part of an identifier. */
  private lazy val identPartEscape: Parser[String] =
    escape ^*? {
      case cps if cps.forall(cp => identPartCriteria.isSatisfiedBy(cp)) => new String(cps.flatMap(Character.toChars(_)).toArray)
    }

  /** An escape sequence that can occur at the start of an identifier. */
  private lazy val identStartEscape: Parser[String] =
    escape ^*? {
      case cps if identStartCriteria.isSatisfiedBy(cps.head) &&
                  cps.tail.forall(cp => identPartCriteria.isSatisfiedBy(cp))
        => new String(cps.flatMap(Character.toChars(_)).toArray)
    }

  private lazy val identPartCriteria =
    identStartCriteria || CodePoint.Category(UnicodeCategory.map(u => u.Mn + u.Mc + u.Nd + u.Pc)) || CodePoint.Values('\u200c', '\u200d')

  private lazy val identStartCriteria =
    CodePoint.Category(UnicodeCategory.map(u => u.Groups.L + u.Nl)) || CodePoint.Values('$', '_')

  // Literals

  lazy val literalExpression: Parser[Expression] = (
    stringLiteralExpression
  | verbatimLiteralExpression
  | (numericLiteralExpression | nullLiteralExpression | booleanLiteralExpression) <~ !:(iriAtom)
  | iriLiteralExpression
  )

  lazy val iriLiteralExpression: Parser[expression.IriLiteral] = {
    /** Defines codepoints which may not appear at the beginning of an IRI in order to disambiguate IRIs from other
      * expression kinds. If you consider how these characters are generally used in a URI, this is not really
      * restrictive at all. */
    val illegalStart = GraphemeParser(Grapheme.SingleCodePoint(CodePoint.Values(
      /* parenthesized expressions: */ '(',
      /*           string literals: */ '\'', '"',
      /*            at-expressions: */ '@',
      /*     elided array elements: */ ',',
      /*      statement terminator: */ ';',
      /*       the unary operators: */ '+', '-', '~', '!'
    )))

    (
      !:(
        keyword ~ !:(iriAtom)
      | commentStart
      )
    ~ !:(illegalStart) ~ iriAtom.+
    ) ^^ { p => expression.IriLiteral(p.parsed.toString) }
  }

  /** An indivisible portion of an IRI literal. */
  protected val iriAtom: Parser[Any] = {
    val ipv4Address = {
      val octet = (
        "25" ~ CodePoint.Range('0', '5')
      | "2"  ~ CodePoint.Range('0', '4') ~ digit
      | "1"  ~ digit.*(2)
      | nonZeroDigit ~ digit
      | digit
      )

      octet ~ ("." ~ octet).*(3)
    }

    val ipv6Address = {
      val h16: Parser[Any] = hexDigit.*(1,4)
      val ch16: Parser[Any] = ":" ~ h16
      val ls32: Parser[Any] = h16 ~ ":" ~ h16 | ipv4Address

      (
        h16                         ~ ch16.*(5) ~ ":" ~ ls32
      |                         ":" ~ ch16.*(5) ~ ":" ~ ls32
      | h16.?                 ~ ":" ~ ch16.*(4) ~ ":" ~ ls32
      | (h16 ~ ch16.?     ).? ~ ":" ~ ch16.*(3) ~ ":" ~ ls32
      | (h16 ~ ch16.*(0,2)).? ~ ":" ~ ch16.*(2) ~ ":" ~ ls32
      | (h16 ~ ch16.*(0,3)).? ~ ":" ~ ch16      ~ ":" ~ ls32
      | (h16 ~ ch16.*(0,4)).?                  ~ "::" ~ ls32
      | (h16 ~ ch16.*(0,5)).?                  ~ "::" ~ h16
      | (h16 ~ ch16.*(0,6)).?                  ~ "::"
      )
    }

    val ipvFutureAddress =
      "v" ~ hexDigit.+ ~ "." ~ CodePoint.Values(
        ('a' to 'z') ++ ('A' to 'Z') ++ ('0' to '9') ++ Set(
          '-', '.', '_', '~',                                     // unreserved
          '!', '$', '&', '\'', '(', ')', '*', '+', ',', ';', '=', // sub-delims
          ':'
        )
      ).+

    /** Valid IRI characters as defined in $iriRfc, less those whose location must be controlled in order to make
      * IRI literals usable in expressions. */
    val char = GraphemeParser(Grapheme.SingleCodePoint(
      CodePoint.Values(
        ('a' to 'z') ++ ('A' to 'Z') ++ ('0' to '9') ++ Set(
        '-', '.', '_', '~',                                    // unreserved
        /*':',*/ '/', '?', '#', /*'[', ']',*/ '@',                     // gen-delims
        '!', '$', '&', '\'', /*'(', ')',*/ '*', '+', /*',', ';',*/ '=' // sub-delims
      ))
    ||CodePoint.Range(0x000A0, 0x0D7FF)
    ||CodePoint.Range(0x0F900, 0x0FDCF)
    ||CodePoint.Range(0x0FDF0, 0x0FFEF)
    ||CodePoint.Range(0x10000, 0x1FFFD)
    ||CodePoint.Range(0x20000, 0x2FFFD)
    ||CodePoint.Range(0x30000, 0x3FFFD)
    ||CodePoint.Range(0x40000, 0x4FFFD)
    ||CodePoint.Range(0x50000, 0x5FFFD)
    ||CodePoint.Range(0x60000, 0x6FFFD)
    ||CodePoint.Range(0x70000, 0x7FFFD)
    ||CodePoint.Range(0x80000, 0x8FFFD)
    ||CodePoint.Range(0x90000, 0x9FFFD)
    ||CodePoint.Range(0xA0000, 0xAFFFD)
    ||CodePoint.Range(0xB0000, 0xBFFFD)
    ||CodePoint.Range(0xC0000, 0xCFFFD)
    ||CodePoint.Range(0xD0000, 0xDFFFD)
    ||CodePoint.Range(0xE0000, 0xEFFFD)
    ))

    /** Valid IRI characters as defined in $iriRfc, but which cannot appear at the end of an IRI expression. */
    val nonTerminalChar = GraphemeParser(Grapheme.SingleCodePoint(CodePoint.Values(',', ';', ':')))

    (
      char
    | nonTerminalChar ~ <>(iriAtom)
    | "%" ~ hexDigit.*(2)
    | "(" ~ (<>(iriAtom) | nonTerminalChar).* ~ ")"
    | "[" ~ (ipv6Address | ipvFutureAddress) ~ "]"
    )
  }

  lazy val nullLiteralExpression: Parser[expression.NullLiteral] = "null" ^^^ expression.NullLiteral()

  lazy val booleanLiteralExpression: Parser[expression.BooleanLiteral] = (
    "true"  ^^^ true
  | "false" ^^^ false
  ) ^* expression.BooleanLiteral

  lazy val numericLiteralExpression: Parser[expression.NumericLiteral] = {
    val decimalLiteral = {
      val signedInteger =         ("+" ^^^ +1d | "-" ^^^ -1d).? ~ (digit.+ ^^(_.parsed)) ^* { p => p._1.getOrElse(1d) * p._2.toString.toDouble }
      val requiredDecimalPart =   "." ~ digit.+ ^^(_.parsed.toString.toDouble)
      val optionalDecimalPart =   ("." ~ digit.* ^^(_.parsed)).?  ^* { p => p.map(_.toString.toDouble).getOrElse(0d) }
      val optionalExponentPart =  (("e" | "E") ~ signedInteger ^*(_._2)).? ^* { p => math.pow(10d, p.map(_.toString.toDouble).getOrElse(0d)) }
      val decimalIntegerLiteral = ("0" | nonZeroDigit ~ digit.*) ^^(_.parsed.toString.toDouble)

      (
        decimalIntegerLiteral ~ optionalDecimalPart ~ optionalExponentPart ^* { p => (p._1 + p._2) * p._3 }
      | requiredDecimalPart ~ optionalExponentPart                         ^* { p => p._1 * p._2 }
      )
    }

    val hexIntegerLiteral =     "0x" ~ (hexDigit.+ ^^(_.parsed)) ^* { p => java.lang.Long.parseLong(p._2.toString, 16).toDouble }

    (hexIntegerLiteral | decimalLiteral) ^* expression.NumericLiteral
  }


  // String Literals

  lazy val verbatimLiteralExpression: Parser[expression.VerbatimLiteral] =
    &:("`") ~> OrderedChoiceParser(
      (1 to 16).reverse.map(n => new String(Array.fill(n)('`'))).map { ticks =>
        ticks ~> ((!:(ticks) ~> unicodeCharacter).* ^^(_.parsed)) <~ ticks
      }
    ) ^* { p => expression.VerbatimLiteral(p.toString) }

  lazy val stringLiteralExpression: Parser[expression.StringLiteral] = {
    val stringPart = escape ^*(_.flatMap(Character.toChars(_))) | !CodePoint.Values(newLineCharValues) ^*(_.chars)

    OrderedChoiceParser(Seq("\"", "'") map { quot =>
      quot ~> (!:(quot) ~> stringPart).* <~ quot ^* { p => expression.StringLiteral(new String(p.flatten.toArray)) }
    })
  }

  /** An argument list, including parentheses. */
  lazy val argumentList: Parser[Seq[Expression]] = {
    val argumentListArguments = <>(expr) ~ (argumentSeparator ~ <>(expr)).* ^* { p => p._1 +: p._2.map(_._2) }

    "(" ~ expressionWhitespace_? ~ argumentListArguments.? ~ expressionWhitespace_? ~ ")" ^*(_._3.getOrElse(Seq()))
  }

  protected lazy val argumentSeparator = expressionWhitespace_? ~ "," ~ expressionWhitespace_? ^^^(())

  lazy val keyword = LiteralSetParser(
                       "break", "case", "catch", "class", "const", "continue", "debugger",
                       "default", "delete", "do", "else", "enum", "export", "extends",
                       "false", "finally", "for", "function", "if", "import", "instanceof",
                       "in", "new", "null", "return", "super", "switch", "this", "throw",
                       "true", "try", "typeof", "var", "void", "while", "with"
                     )

  /** Zero or more space characters or comments. */
  lazy val expressionWhitespaceNoNewline_? = (spaceChar | comment).*

  /** Zero or more whitespace characters or comments. */
  lazy val expressionWhitespace_? = expressionWhitespace.?
  lazy val expressionWhitespace = (whitespace | comment).+
}
