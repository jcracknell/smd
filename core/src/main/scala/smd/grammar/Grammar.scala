package smd
package grammar

// TODO: add helpers to eliminate specific parser imports
import smd.parsing.{Parser, Parsers, OrderedChoiceParser, LiteralSetParser, GraphemeParser}
import smd.markdown.{Block, Inline}
import smd.expression.Expression

object Grammar extends Grammar

trait Grammar extends Parsers {
  protected def parse[A](parser: Parser[A], extents: Seq[CharSequence]): A =
    // TODO: error checking
    parser.parse(extents).product

  lazy val document: Parser[markdown.Document] = blocks ^* markdown.Document

  //^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  // BLOCKS
  //vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv

  lazy val blocks: Parser[Seq[Block]] =
    interBlock.? ~> repSep(0, block, interBlock.?) <~ interBlock.? ^* { _.collect { case Left(b) => b } }

  lazy val block: Parser[Block] = (
    heading
  | blockquote
  | unorderedList
  | orderedList
  | reference
  | paragraph
  )

  /** Zero or more blank lines interleaved with comments. */
  lazy val interBlock = repSep(0, blankLines_?, spaceChars_? ~ comment) ^^(_.parsed)

  lazy val reference: Parser[markdown.Reference] = {
    // Divergence from spec: we accept any number of spaces, as we do not support indented code blocks
    val ref = spaceChars_? ~> referenceId <~ ":" ~ blockWhitespaceOrComments

    val blockArgumentList: Parser[Seq[Expression]] = {
      val separator = blockWhitespaceOrComments ~ "," ~ blockWhitespaceOrComments
      repSep(1, leftHandSideExpression, separator) ^* { _.collect { case Left(e) => e } }
    }

    ref ~ (blockArgumentList | argumentList) ^~ { (r, as) => markdown.Reference(r, as) }
  }

  lazy val orderedList: Parser[markdown.OrderedList] = {
    val enumerator = (
      nonIndentSpace_? ~> digit.+ <~ "." ~ spaceChar.*
      ^^ { r => try { Integer.parseInt(r.parsed.toString) } catch { case _: Throwable => 1 } }
    )

    genList(enumerator)(
      is => markdown.OrderedList.Tight(
        is map { case (_, inlines) => markdown.OrderedList.Item(inlines) },
        markdown.OrderedList.CounterStyle.Arabic
      ),
      is => markdown.OrderedList.Loose(
        is map { case (_, blocks) => markdown.OrderedList.Item(blocks) },
        markdown.OrderedList.CounterStyle.Arabic
      )
    )
  }

  lazy val unorderedList: Parser[markdown.UnorderedList] = {
    /** A bullet must be followed by at least one space to avoid confusion with emphasis, negative numbers, etc. */
    val bullet = nonIndentSpace_? ~ ("*" | "-" | "+") ~ spaceChar.+

    genList(bullet)(
      is => markdown.UnorderedList.Tight(
        is map { case (_, inlines) => markdown.UnorderedList.Item(inlines) }
      ),
      is => markdown.UnorderedList.Loose(
        is map { case (_, blocks) => markdown.UnorderedList.Item(blocks) }
      )
    )
  }

  /** Generic grammar for a list.
    *
    * @param marker parser for a list marker.
    * @param tight function for creating a tight list from a sequence of marker-content pairs.
    * @param loose function for creating a loose list from a sequence of marker-content pairs.
    * @tparam M product type of the marker parser.
    * @tparam L resulting list type.
    */
  protected def genList[M, L <: markdown.List](marker: Parser[M])(
    tight: Seq[(M, Seq[markdown.Inline])] => L,
    loose: Seq[(M, Seq[markdown.Block])] => L
  ): Parser[L] = {
    /** A line starting with a list marker. */
    val markedLine = marker ~ blockLine_?
    /** A line not starting with a list marker. Consumes an optional indent at the beginning of the line. */
    val unmarkedLine = ?!(marker) ~ indent.? ~> blockLine

    /** The initial block of a list item, beginning with a marker. */
    val itemInitialBlock = markedLine ~ unmarkedLine.* ^~ { (m, a, b) => (m, a +: b) }
    /** A subsequent block of a list item, indented and not starting with a list marker.
      * The indent is consumed by noMarkerLine. */
    val itemSubsequentBlock = interBlock ~ ?=(indent) ~ unmarkedLine.+ ^~ { (a, _, b) => a +: b }

    /** A 'tight' list item is a list item which contains only a single block. */
    val itemTight = itemInitialBlock
    /** A 'loose' list item can contain subsequent blocks of content. */
    val itemLoose = itemInitialBlock ~ itemSubsequentBlock.* ^* { case ((m, a), bs) => (m, a ++ bs.flatten) }

    /** A 'tight' list is a sequence of tight list items not followed by loose content. */
    val tightList = (
      itemTight.+ <~ ?!(interBlock ~ (marker | indent))
      ^* { items => tight(items map { case (m, ls) => (m, parse(blockInlines_?, ls)) }) }
    )

    val looseList = (
      repSep(1, itemLoose, interBlock.?) ^* { p => loose(p.collect { case Left((m, ls)) => (m, parse(blocks, ls)) }) }
    )

    tightList | looseList
  }

  lazy val blockquote: Parser[markdown.Blockquote] = {
    val announcedLine = ">" ~ " ".? ~> blockLine_?
    val blockquoteBlock = announcedLine ~ (announcedLine | blockLine).* ^~ { (init, subs) => parse(&(blocks), init +: subs) }

    repSep(1, blockquoteBlock, interBlock) ^* { p => markdown.Blockquote(p.collect({ case Left(b) => b }).flatten) }
  }

  lazy val heading: Parser[markdown.Heading] =
    spaceChars_? ~> "#".*>=(1) ~ blockInlines ^~ ((h, is) => markdown.Heading(h.length, is))

  lazy val paragraph: Parser[markdown.Paragraph] =
    blockInlines ^* markdown.Paragraph

  lazy val blockInlines: Parser[Seq[Inline]] =
    blockWhitespaceOrComments.? ~> (inline.+ <~ blockWhitespaceOrComments.?).+ ^*(_.flatten)

  lazy val blockInlines_? : Parser[Seq[Inline]] =
    blockWhitespaceOrComments.? ~> (inline.+ <~ blockWhitespaceOrComments.?).* ^*(_.flatten)

  lazy val blockLine = ?!(blankLine) ~> blockLine_?

  lazy val blockLine_? : Parser[CharSequence] = {
    val atomic = (
      ?!(specialChar) ~ unicodeCharacter
    | inlineExpression
    | comment
    | link
    | autoLink
    | code
    | unicodeCharacter
    )

    (?!(newLine) ~ atomic).* ~ newLine.? ^^(_.parsed)
  }

  //^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  // INLINES
  //vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv

  lazy val inline: Parser[Inline] = (
    text
  | lineBreak
  | space
  | strong
  | emphasis
  | quoted
  | link
  | autoLink
  | code
  | entity
  | inlineExpression
  | symbol
  )

  /** A link of the form `[link text][optional refid](url, args)`. */
  lazy val link: Parser[markdown.Link] = {
    val label = "[" ~> (?!("]") ~> &(inline)).* <~ "]"

    label ~ referenceId.? ~ argumentList.? ^~ { (lbl, ref, args) => markdown.Link(lbl, ref, args.getOrElse(Seq())) }
  }

  /** A reference id enclosed in square brackets. */
  lazy val referenceId: Parser[markdown.ReferenceId] = (
    "[" ~> ((?!(CodePoint.Values(newLineCharValues + ']')) ~ unicodeCharacter).* ^^ (_.parsed)) <~ "]"
  ) ^* { p => markdown.ReferenceId(p.toString) }

  lazy val autoLink: Parser[markdown.AutoLink] =
    // TODO: decoding? more forgiving?
    "<" ~ ?=(englishAlpha.+ ~ ":") ~> (iriLiteralExpression ^* { iri => markdown.AutoLink(iri.value) }) <~ ">"

  lazy val strong = "**" ~> (?!("**") ~> &(inline)).+ <~ "**" ^* markdown.Strong

  lazy val emphasis = "*" ~> (?!("*") ~> &(inline) | &(strong)).+ <~ "*" ^* markdown.Emphasis

  lazy val lineBreak =
    blockWhitespaceOrComments ~ "\\" ~ ?=(blankLine) ~ blockWhitespaceOrComments ^^^ markdown.LineBreak()

  lazy val text: Parser[markdown.Text] = {
    val apos = "'" ~ Grapheme.Category(UnicodeCategory.Groups.Letter ++ UnicodeCategory.Groups.Number)

    normalChar.+ ~ apos.* ^^ { r => markdown.Text(r.parsed.toString)}
  }

  /** Any non-empty combination of comments and whitespace not leaving or at the end of a block. */
  lazy val space = blockWhitespaceOrComments ~ ?!(blankLine) ^^^ markdown.Space()

  lazy val entity = escape ^* markdown.Entity

  lazy val quoted = OrderedChoiceParser(
    Seq(
      "\"" -> markdown.Quoted.QuoteKind.Double,
      "'"  -> markdown.Quoted.QuoteKind.Single
    ) map { case (quot, kind) =>
      quot ~> (?!(quot) ~> &(inline)).* <~ quot ^* { is => markdown.Quoted(is, kind) }
    }
  )

  /** Backtick-enclosed code not leaving a block. */
  lazy val code: Parser[markdown.Code] =
    ?=("`") ~> OrderedChoiceParser((1 to 16).map(n => "".padTo(n, '`')).map { ticks =>
      val content = ?!("`") ~ blockWhitespace.? ~ (?!(whitespace | ticks) ~ unicodeCharacter ~ blockWhitespace.?).+
      ticks ~> (content ^^ { _.parsed }) <~ ticks
    }) ^* { p => markdown.Code(p.toString) }

  lazy val inlineExpression = ?=("@") ~> &(leftHandSideExpression) <~ ";".? ^* markdown.InlineExpression

  lazy val symbol = CodePoint.Values(specialCharValues) ^* { p => markdown.Symbol(p.charSequence.toString) }

  /** Any non-empty combination of comments and whitespace not consuming a blank line. */
  protected lazy val blockWhitespaceOrComments = (
    blockWhitespace ~ (comment ~ blockWhitespace.?).*
  | (comment ~ blockWhitespace.?).+
  )

  /** Any non-empty amount of whitespace not consuming a blank line. */
  protected lazy val blockWhitespace = (
    spaceChar.+ ~ (newLine ~ spaceChars_? ~ ?!(blankLine)).?
  | newLine ~ spaceChars_? ~ ?!(blankLine)
  )

  /** A normal character; not a special or whitespace character. */
  protected lazy val normalChar = !Grapheme.SingleCodePoint(CodePoint.Values(specialCharValues ++ whitespaceCharValues))

  /** A special character; a character which can denote the beginning of a structural element. */
  protected lazy val specialChar = CodePoint.Values(specialCharValues)

  /** The set of special character values; characters which can denote the beginnig of a structural element. */
  protected lazy val specialCharValues = Set(
    '*',        // strong, emphasis
    '\'', '\"', // quotes
    '`',        // ticks
    '/',        // comments
    '\\',       // escape sequence
    '[', ']',   // labels
    '<', '>',   // autolinks
    '|',        // table cell delimiter
    '@'         // expression start
  )

  //^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  // EXPRESSIONS
  //vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv

  def expr: Parser[Expression] = conditionalExpression

  lazy val conditionalExpression: Parser[Expression] = (
    logicalOrExpression
  ~ ( expressionWhitespace_? ~ "?" ~ expressionWhitespace_?
    ~ &(conditionalExpression)
    ~ expressionWhitespace_? ~ ":" ~ expressionWhitespace_?
    ~ &(conditionalExpression)
    ^*{ p => (p._4, p._8) }
    ).? ^* {
      case (i, Some((t, e))) => expression.Conditional(i, t, e)
      case (e, None) => e
    }
  )

  lazy val logicalOrExpression = binOp(logicalAndExpression,  "||" ~ ?!("=")       ^^^ expression.LogicalOr)

  lazy val logicalAndExpression = binOp(bitwiseOrExpression,  "&&" ~ ?!("=")       ^^^ expression.LogicalAnd)

  lazy val bitwiseOrExpression = binOp(bitwiseXOrExpression,  "|"  ~ ?!("|" | "=") ^^^ expression.BitwiseOr)

  lazy val bitwiseXOrExpression = binOp(bitwiseAndExpression, "^"  ~ ?!("=")       ^^^ expression.BitwiseXOr)

  lazy val bitwiseAndExpression = binOp(equalityExpression,   "&"  ~ ?!("&" | "=") ^^^ expression.BitwiseAnd)

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
  | unaryOp("typeof" ~ ?!(identifierExpressionPart), expression.TypeOf)
  | unaryOp("delete" ~ ?!(identifierExpressionPart), expression.Delete)
  | unaryOp("void"   ~ ?!(identifierExpressionPart), expression.Void)
  | postfixExpression
  )

  private def unaryOp(op: Parser[Any], builder: Expression => Expression): Parser[Expression] =
    op ~ expressionWhitespace_? ~> &(unaryExpression) ^*(builder(_))

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
    val dynamicProperty = "[" ~ expressionWhitespace_? ~> &(expr) <~ expressionWhitespace_? ~ "]"

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
  | "(" ~ expressionWhitespace_? ~> &(expr) <~ expressionWhitespace_? ~ ")"
  )

  lazy val arrayLiteralExpression: Parser[expression.ArrayLiteral] = {
    /** A non-elided array element preceded by any number of elided elements. */
    val subsequentArrayElement =
      argumentSeparator.+ ~ expressionWhitespace_? ~ &(expr) ^* { case (seps, _, e) => seps.tail.map(_ => expression.Elided()) :+ e }

    val arrayElements = (
      (
        &(expr) ~ subsequentArrayElement.* ^* { case (e, ses) => e +: ses.flatten }
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
      propertyName ~ expressionWhitespace_? ~ ":" ~ expressionWhitespace_? ~ &(expr) ^* { p => (p._1, p._5) }

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
    ?!(keyword ~ ?!(identifierExpressionPart)) ~>
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
  | (numericLiteralExpression | nullLiteralExpression | booleanLiteralExpression) <~ ?!(iriAtom)
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
      ?!(
        keyword ~ ?!(iriAtom)
      | commentStart
      )
    ~ ?!(illegalStart) ~ iriAtom.+
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
    | nonTerminalChar ~ &(iriAtom)
    | "%" ~ hexDigit.*(2)
    | "(" ~ (&(iriAtom) | nonTerminalChar).* ~ ")"
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
    ?=("`") ~> OrderedChoiceParser(
      (1 to 16).reverse.map(n => new String(Array.fill(n)('`'))).map { ticks =>
        ticks ~> ((?!(ticks) ~> unicodeCharacter).* ^^(_.parsed)) <~ ticks
      }
    ) ^* { p => expression.VerbatimLiteral(p.toString) }

  lazy val stringLiteralExpression: Parser[expression.StringLiteral] = {
    val stringPart = escape ^*(_.flatMap(Character.toChars(_))) | !CodePoint.Values(newLineCharValues) ^*(_.chars)

    OrderedChoiceParser(Seq("\"", "'") map { quot =>
      quot ~> (?!(quot) ~> stringPart).* <~ quot ^* { p => expression.StringLiteral(new String(p.flatten.toArray)) }
    })
  }

  /** An argument list, including parentheses. */
  lazy val argumentList: Parser[Seq[Expression]] = {
    val argumentListArguments = &(expr) ~ (argumentSeparator ~ &(expr)).* ^* { p => p._1 +: p._2.map(_._2) }

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

  //^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  // COMMON PRODUCTIONS
  //vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv

  /** An escape sequence. Yields a sequence of code points. */
  lazy val escape: Parser[Seq[Int]] = {
    val characterEscape = LiteralSetParser(
      Map(
        "\"" -> '\"',    "\"" -> '\"',    "t"  -> '\t',    "n"  -> '\n',
        "r"  -> '\r',    "\\" -> '\\',    "b"  -> '\b',    "f"  -> '\f',
        "v"  -> '\u000b'
      ) mapValues { c => Seq(c.toInt) }
    )
    val numericEscape = (
      "#" ~> (digit.*(1,6) ^^ { r => Seq(Integer.parseInt(r.parsed.toString, 10)) }) <~ ";".?
    | "#".? ~ ("u" | "x") ~> (hexDigit.*(1,6) ^^ { r => Seq(Integer.parseInt(r.parsed.toString, 16)) }) <~ ";".?
    )
    val namedEscape = LiteralSetParser(NamedEntity.entities.values.map(e => (e.name, e.codePoints))) <~ ";"
    val literalEscape = !CodePoint.Values(newLineCharValues) ^* { _.codePoints.map(_.value) }

    "\\" ~> (characterEscape | numericEscape | namedEscape | literalEscape)
  }

  /** A single or multi-line comment. */
  lazy val comment = {
    val multiLineComment =  "/*" ~ (?!("*/") ~ unicodeCharacter).* ~ "*/"
    val singleLineComment = "//" ~ line_?

    singleLineComment | multiLineComment
  }

  protected lazy val commentStart = "//" | "/*"

  /** The (remainder) of the the current line, including the newline sequence. */
  protected lazy val line_? = (?!(newLine) ~ unicodeCharacter).* ~ newLine.?

  /** Zero or more blank lines. */
  protected lazy val blankLines_? = (spaceChars_? ~ newLine).* ~ (spaceChars_? ~ EOF).? ^^(_.parsed)

  /** Zero or more space characters followed by a newline or the end of the input.
    * This parser should _never_ be repeated. */
  protected lazy val blankLine = spaceChars_? ~ (newLine | EOF) ^^ (_.parsed)

  /** A tab or four spaces. */
  protected lazy val indent = "\t" | "    "
  /** Up to three space characters. */
  protected lazy val nonIndentSpace_? = " ".*(0,3)

  protected lazy val hexDigit =          CodePoint.Values(('0' to '9') ++ ('a' to 'f') ++ ('A' to 'F'))
  protected lazy val digit =             CodePoint.Values('0' to '9')
  protected lazy val nonZeroDigit =      CodePoint.Values('1' to '9')
  protected lazy val englishAlpha =      englishLowerAlpha | englishUpperAlpha
  protected lazy val englishLowerAlpha = CodePoint.Range('a', 'z')
  protected lazy val englishUpperAlpha = CodePoint.Range('A', 'Z')

  /** A space character or newline sequence. */
  protected lazy val whitespace = spaceChar | newLine
  protected lazy val whitespaceCharValues = spaceCharValues ++ newLineCharValues
  /** A valid newline sequence. */
  protected lazy val newLine =    "\r\n" | CodePoint.Values(newLineCharValues)
  protected lazy val newLineCharValues = Set('\n', '\r', '\u2028', '\u2029')
  protected lazy val spaceChars_? = spaceChar.*
  protected lazy val spaceChar =  CodePoint.Values(' ', '\t')
  protected lazy val spaceCharValues = Set(' ', '\t')

  /** Any single unicode grapheme. */
  protected lazy val unicodeCharacter = Grapheme.Any
}