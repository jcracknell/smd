package smd
package grammar

import smd.parsing.{LiteralSetParser, OrderedChoiceParser}

trait ExpressionProductions extends CommonProductions {
  def expr: Parser[Expression] = conditionalExpression

  lazy val conditionalExpression: Parser[Expression] = (
    logicalOrExpression
  ~ ( expressionWhitespace ~ "?" ~ expressionWhitespace
    ~ <>(conditionalExpression)
    ~ expressionWhitespace ~ ":" ~ expressionWhitespace
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
      expressionWhitespace ~ ops ~ expressionWhitespace ~ operand ^* { case (_, op, _, rhs) => (op, rhs) }
    ).* ^* { case (lhs, ops) => (lhs /: ops) { (body, op) => op._1(body, op._2) } }

  lazy val unaryExpression: Parser[Expression] =
    "!"  ~ expressionWhitespace ~> <>(unaryExpression) ^* expression.LogicalNot      |
    "--" ~ expressionWhitespace ~> <>(unaryExpression) ^* expression.PrefixDecrement |
    "-"  ~ expressionWhitespace ~> <>(unaryExpression) ^* expression.Negative        |
    "++" ~ expressionWhitespace ~> <>(unaryExpression) ^* expression.PrefixIncrement |
    "+"  ~ expressionWhitespace ~> <>(unaryExpression) ^* expression.Positive        |
    "~"  ~ expressionWhitespace ~> <>(unaryExpression) ^* expression.BitwiseNot      |
    "typeof" ~ !:(identifierExpressionPart) ~ expressionWhitespace ~> <>(unaryExpression) ^* expression.Typeof |
    "delete" ~ !:(identifierExpressionPart) ~ expressionWhitespace ~> <>(unaryExpression) ^* expression.Delete |
    "void"   ~ !:(identifierExpressionPart) ~ expressionWhitespace ~> <>(unaryExpression) ^* expression.Void |
    leftHandSideExpression

  lazy val postfixExpression: Parser[Expression] =
    leftHandSideExpression ~ ( expressionWhitespaceNoNewline ~>
      "--" ^^^ expression.PostfixDecrement |
      "++" ^^^ expression.PostfixIncrement
    ).? ^* {
      case (body, Some(builder)) => builder(body)
      case (body, _) => body
    }

  lazy val leftHandSideExpression: Parser[Expression] = {
    val staticProperty =  "." ~ expressionWhitespace ~> identifier
    val dynamicProperty = "[" ~ expressionWhitespace ~> <>(expr) <~ expressionWhitespace ~ "]"

    atExpression ~ (expressionWhitespace ~ (
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
  | "(" ~ expressionWhitespace ~> <>(expr) <~ expressionWhitespace ~ ")"
  )

  lazy val arrayLiteralExpression: Parser[expression.ArrayLiteral] = {
    /** A non-elided array element preceded by any number of elided elements. */
    val subsequentArrayElement =
      argumentSeparator.+ ~ expressionWhitespace ~ <>(expr) ^* { case (seps, _, e) => seps.tail.map(_ => expression.Elided()) :+ e }

    val arrayElements = (
      (
        <>(expr) ~ subsequentArrayElement.* ^* { case (e, ses) => e +: ses.flatten }
      | subsequentArrayElement.+            ^* { p => expression.Elided() +: p.flatten } // initial element elided
      ).?
    <~ argumentSeparator.*
    ^*(_.getOrElse(Seq()))
    )

    "[" ~ expressionWhitespace ~> arrayElements <~ expressionWhitespace ~ "]" ^* expression.ArrayLiteral
  }

  lazy val objectLiteralExpression: Parser[expression.ObjectLiteral] = {
    val objectPropertyAssignment = (
      (stringLiteral | numericLiteral ^*(_.toString) | identifier)
    ~ expressionWhitespace ~ ":" ~ expressionWhitespace
    ~ <>(expr)
    ^* { p => (p._1, p._5) }
    )

    val objectPropertyAssignments = (
      objectPropertyAssignment
    ~ (argumentSeparator ~ objectPropertyAssignment).*
    ~ argumentSeparator.?
    ^* { p => p._1 +: p._2.map(_._2) }
    )

    (
      "{" ~ expressionWhitespace ~> objectPropertyAssignments.? <~ expressionWhitespace ~ "}"
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

  lazy val literalExpression = (
    nullLiteralExpression
  | booleanLiteralExpression
  | numericLiteralExpression
  | stringLiteralExpression
  )

  lazy val nullLiteralExpression = nullLiteral ^^^ expression.NullLiteral()
  lazy val nullLiteral = "null"

  lazy val booleanLiteralExpression = booleanLiteral ^* expression.BooleanLiteral

  lazy val booleanLiteral = (
    "true"  ^^^ true
  | "false" ^^^ false
  )

  lazy val numericLiteralExpression = numericLiteral ^* expression.NumericLiteral

  lazy val numericLiteral = {
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

    decimalLiteral | hexIntegerLiteral
  }


  // String Literals

  lazy val stringLiteralExpression: Parser[expression.StringLiteral] = (
    quotedStringLiteralExpression
  | verbatimStringLiteralExpression
  )

  protected lazy val stringLiteral = (
    quotedStringLiteral
  | verbatimStringLiteral
  )

  lazy val verbatimStringLiteralExpression = verbatimStringLiteral ^* expression.VerbatimStringLiteral

  protected lazy val verbatimStringLiteral = (
    &:("`") ~ OrderedChoiceParser(
      (1 to 16).reverse.map(n => new String(Array.fill(n)('`'))).map { ticks =>
        ticks ~> ((!:(ticks) ~ unicodeCharacter).* ^^(_.parsed)) <~ ticks
      }
    ) ^*(_._2.toString)
  )

  lazy val quotedStringLiteralExpression = quotedStringLiteral ^* expression.QuotedStringLiteral

  protected lazy val quotedStringLiteral = {
    val stringPart = escape ^*(_.flatMap(Character.toChars(_))) | !CodePoint.Values(newLineCharValues) ^*(_.chars)

    OrderedChoiceParser(Seq("\"", "'") map { quot =>
      quot ~> (!:(quot) ~> stringPart).* <~ quot ^* { p => new String(p.flatten.toArray) }
    })
  }

  /** An argument list, including parentheses. */
  lazy val argumentList: Parser[Seq[Expression]] = "(" ~ expressionWhitespace ~ argumentListArguments.? ~ expressionWhitespace ~ ")" ^*(_._3.getOrElse(Seq()))

  private lazy val argumentListArguments = <>(expr) ~ (argumentSeparator ~ <>(expr)).* ^* { p => p._1 +: p._2.map(_._2) }

  protected lazy val argumentSeparator = expressionWhitespace ~ "," ~ expressionWhitespace ^^^(())

  lazy val keyword = LiteralSetParser(
                       "break", "case", "catch", "class", "const", "continue", "debugger",
                       "default", "delete", "do", "else", "enum", "export", "extends",
                       "false", "finally", "for", "function", "if", "import", "instanceof",
                       "in", "new", "null", "return", "super", "switch", "this", "throw",
                       "true", "try", "typeof", "var", "void", "while", "with"
                     )

  /** Zero or more space characters or comments. */
  lazy val expressionWhitespaceNoNewline = (spaceChar | comment).*

  /** Zero or more whitespace characters or comments. */
  lazy val expressionWhitespace = (whitespace | comment).*
}
