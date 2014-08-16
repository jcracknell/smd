package smd
package grammar

// TODO: add helpers to eliminate specific parser imports
import smd.dom.{Block, Expression, Inline}
import smd.parsing._

object Grammar extends Grammar

trait Grammar extends Parsers {
  protected def parseExtents[A](parser: Parser[A], extents: Seq[CharSequence]): A =
    // TODO: error checking
    parser.parse(extents).product

  lazy val document: Parser[dom.Document] = blocks ^* dom.Document

  //region Blocks

  lazy val blocks: Parser[Seq[Block]] =
    interBlock_? ~> repSep(0, block, interBlock_?) <~ interBlock_? ^* { _.collect { case Left(b) => b } }

  lazy val block: Parser[Block] = (
    heading
  | blockquote
  | unorderedList
  | orderedList
  | reference
  | definitionList
  | table
  | expressionBlock
  | paragraph
  )

  /** Zero or more blank lines interleaved with comments. */
  lazy val interBlock_? = repSep(0, blankLines_?, spaceChars_? ~ comment) ^^(_.parsed)

  lazy val reference: Parser[dom.Reference] = {
    // Divergence from spec: we accept any number of spaces, as we do not support indented code blocks
    val ref = spaceChars_? ~> referenceId <~ ":" ~ sp

    ref ~ (parenthesizedArgumentList | argumentList) <~ sp.? ~ blankLine ^~ {
      (r, as) => dom.Reference(r, as)
    }
  }

  //region Lists

  val unorderedList = new ListGrammar[dom.UnorderedList] {
    type MarkerProduct = Any
    lazy val marker = "*" | "-" | "+"

    def mkLoose(items: Seq[(MarkerProduct, Seq[Block])]): dom.UnorderedList =
      dom.UnorderedList.Loose(items map { case (_, children) => dom.UnorderedList.Item(children) })

    def mkTight(items: Seq[(MarkerProduct, Seq[Inline])]): dom.UnorderedList =
      dom.UnorderedList.Tight(items map { case (_, children) => dom.UnorderedList.Item(children) })
  }

  lazy val orderedList: Parser[dom.OrderedList] = {
    // Arabic numerals must come first as the default in the event of an initial placeholder marker.
    val numeralStyles = Seq(
      (dom.OrderedList.NumeralStyle.Arabic,     """[0-9]+""".r.p                    ),
      (dom.OrderedList.NumeralStyle.LowerRoman, """[mdclxvi][mdclxviMDCLXVI]*""".r.p),
      (dom.OrderedList.NumeralStyle.UpperRoman, """[MDCLXVI][mdclxviMDCLXVI]*""".r.p),
      (dom.OrderedList.NumeralStyle.LowerAlpha, """[a-z][a-zA-Z]*""".r.p            ),
      (dom.OrderedList.NumeralStyle.UpperAlpha, """[A-Z][a-zA-Z]*""".r.p            )
    )

    val separatorStyles = Seq(
      (dom.OrderedList.SeparatorStyle.TrailingDot,          ε,     ".".p),
      (dom.OrderedList.SeparatorStyle.TrailingParenthesis,  ε,     ")".p),
      (dom.OrderedList.SeparatorStyle.EnclosingParentheses, "(".p, ")".p)
    )

    val counterName_? = (("""[\p{Lower}\p{Upper}\p{Digit}_-]+""".r <~ ":") ^* (_.matched)).?

    // For performance reasons we use a regex to quickly check if there is a valid counter ahead
    // This is also a relatively concise and comprehensible reference for the counter grammar.
    val counterish = 
      """(?iux)                             # ignore case, unicode character classes, comments
      [\ ]{0,3}                             # non-indent space
      [(]?                                  # begin separator
      ([\p{Lower}\p{Upper}\p{Digit}_-]+:)?  # optional counter name
      (
        \#                                  # auto-number
      | [0-9]+                              # arabic
      | [a-z]+                              # alpha/roman
      )
      [.)]                                  # end separator
      [\t\ ]                                # must be followed by at least one space
      """.r

    // TODO: is there a sane way we can enforce the same counter name across all list elements?
    ?=(counterish) ~> |<< {
      for {
        (numeralStyle, numeral)               <- numeralStyles
        (separatorStyle, sepBefore, sepAfter) <- separatorStyles
      } yield new ListGrammar[dom.OrderedList] {
        type MarkerProduct = (Option[String], CharSequence)
        lazy val marker = sepBefore ~> counterName_? ~ ("#" | numeral ^^(_.parsed)) <~ sepAfter

        def mkLoose(items: Seq[(MarkerProduct, Seq[Block])]): dom.OrderedList =
          items.head match { case ((counterName, start), _) =>
            dom.OrderedList.Loose(
              dom.OrderedList.Counter(numeralStyle, separatorStyle, numeralStyle.decode(start), counterName),
              items map { case (_, children) => dom.OrderedList.Item(children) }
            )
          }

        def mkTight(items: Seq[(MarkerProduct, Seq[Inline])]): dom.OrderedList =
          items.head match { case ((counterName, start), _) =>
            dom.OrderedList.Tight(
              dom.OrderedList.Counter(numeralStyle, separatorStyle, numeralStyle.decode(start), counterName),
              items map { case (_, children) => dom.OrderedList.Item(children) }
            )
          }
      }
    }
  }

  lazy val definitionList: Parser[dom.DefinitionList] = rule {
    val term = blockLine

    val defGrammar = new ListItemGrammar {
      type MarkerProduct = Any
      lazy val marker = ":" | "~"
      // Indent is required for unmarked lines
      protected override lazy val unmarkedLine = ?!(spacedMarker) ~ indent ~> blockLine
    }

    val itemTight = term ~ defGrammar.itemTight.+
    val itemLoose = term ~ (interBlock_? ~> defGrammar.itemLoose).+

    val tightList = itemTight.+ <~ ?!(interBlock_? ~ (indent | defGrammar.marker | itemLoose)) ^* {
      _ map { case (rawTerm, rawDefs) =>
        val term = parseExtents(blockInlines_?, Seq(rawTerm))
        val defs = rawDefs map { case (_, rawDef) => dom.DefinitionList.Definition(parseExtents(blockInlines_?, rawDef)) }
        dom.DefinitionList.Item(dom.DefinitionList.Term(term), defs: _*)
      } match { case items =>
        dom.DefinitionList.Tight(items)
      }
    }

    val looseList = repSep(1, itemLoose, interBlock_?) ^* {
      _ collect { case Left((rawTerm, rawDefs)) =>
        val term = dom.DefinitionList.Term(parseExtents(blockInlines_?, Seq(rawTerm)))
        val defs = rawDefs map { case (_, rawDef) => dom.DefinitionList.Definition(parseExtents(blocks, rawDef))}
        dom.DefinitionList.Item(term, defs: _*)
      } match { case items =>
        dom.DefinitionList.Loose(items)
      }
    }

    tightList | looseList
  }

  /** Generic grammar for lists. */
  abstract class ListGrammar[+L <: dom.List] extends Parser[L] with ListItemGrammar {
    def mkLoose(items: Seq[(MarkerProduct, Seq[dom.Block])]): L
    def mkTight(items: Seq[(MarkerProduct, Seq[dom.Inline])]): L

    def parse(context: ParsingContext): ParsingResult[L] = list.parse(context)

    protected lazy val list: Parser[L] = {
      // A 'tight' list is a sequence of tight list items not followed by loose content.
      val tightList = itemTight.+ <~ ?!(interBlock_? ~ (indent | itemLoose)) ^* { rawItems =>
        val items = rawItems map { case (m, extents) => (m, parseExtents(blockInlines_?, extents)) }
        mkTight(items)
      }

      val looseList = repSep(1, itemLoose, interBlock_?) ^* { p =>
        val items = p collect { case Left((m, extents)) => (m, parseExtents(blocks, extents)) }
        mkLoose(items)
      }

      tightList | looseList
    }
  }

  /** Generic grammar for list items. */
  trait ListItemGrammar {
    type MarkerProduct
    def marker: Parser[MarkerProduct]

    lazy val spacedMarker = nonIndentSpace_? ~> marker <~ spaceChar.+

    // A line starting with a list marker.
    protected lazy val markedLine = spacedMarker ~ blockLine_?
    // A line not starting with a list marker. Consumes an optional indent at the beginning of the line.
    protected lazy val unmarkedLine = ?!(spacedMarker) ~ indent.? ~> blockLine

    // The initial block of a list item, beginning with a marker.
    protected lazy val itemInitialBlock = markedLine ~ unmarkedLine.* ^~ { (m, a, b) => (m, a +: b) }
    // A subsequent block of a list item, indented and not starting with a list marker.
    // The indent is consumed by noMarkerLine.
    protected lazy val itemSubsequentBlock = interBlock_? ~ ?=(indent) ~ unmarkedLine.+ ^~ { (a, _, b) => a +: b }

    /** A 'tight' list item is a list item which contains only a single block. */
    lazy val itemTight = itemInitialBlock
    /** A 'loose' list item can contain subsequent blocks of content. */
    lazy val itemLoose = itemInitialBlock ~ itemSubsequentBlock.* ^* { case ((m, a), bs) => (m, a ++ bs.flatten) }
  }

  //endregion

  lazy val table = {
    val ` || ` = spaceChars_? ~> "|".p.+                      <~ spaceChars_? ^* { _.length }
    val ` |+ ` = spaceChars_? ~> CodePoint.Values('|', '+').+ <~ spaceChars_? ^* { _.length }

    // We define rules to consume empty cells consisting only of comments and whitespace at
    // the start and end of a row
    val rowStart = rule { (spaceChars_? ~ multiLineComment).* }
    val rowEnd   = commentLine

    val  |> = rule { rowStart ~> ` || `                                       }
    val <|> = rule {             ` || `   <~ ?!(rowEnd)                       }
    val <|  = rule {             ` || `.? <~    rowEnd  ^* { _.getOrElse(1) } }
    val  +> = rule { rowStart ~> ` |+ `                                       }
    val <+> = rule {             ` |+ `   <~ ?!(rowEnd)                       }
    val <+  = rule {             ` |+ `.? <~    rowEnd  ^* { _.getOrElse(1) } }

    val align = {
      val `--` = CodePoint.Values('-', '=').+
      val `:`  = ":".p

      ( `:`   ~ `--` ~ `:`  ^^^ dom.Table.CellAlignment.Center
      |         `--` ~ `:`  ^^^ dom.Table.CellAlignment.Right
      | `:`.? ~ `--`        ^^^ dom.Table.CellAlignment.Left
      )
    }

    val cellContent = (?!(` || ` | rowEnd) ~ blockAtom).* ^^ { _.parsed }

    val alignRow = +> ~> (align       ~  <+>).* ~ align       ~ <+ ^~ { (as, fa, fs) => as :+ (fa, fs) }
    val row      = |> ~> (cellContent ~  <|>).* ~ cellContent ~ <| ^~ { (cs, fc, fs) => cs :+ (fc, fs) }

    val commentRows_? = (
      (spaceChars_? ~ multiLineComment).+ ~ (blankLine | singleLineComment)
    | spaceChars_? ~ singleLineComment
    ).*

    val headRows = repSepS(1, commentRows_?, ?!(alignRow) ~> row)
    val bodyRows = repSepS(1, commentRows_?,                 row)

    def mkRows(rows: Seq[Seq[(CharSequence, Int)]], alignments: List[dom.Table.CellAlignment]) =
      rows map { row =>
        val cells =  (row.toList, alignments) unfoldRight {
          case ((c, span) :: cs, as) =>
            val alignment = as.headOption.getOrElse(dom.Table.CellAlignment.Left)
            val cell = dom.Table.Cell(alignment, span, parseExtents(blockInlines_?, Seq(c)))
            // Emit the cell, dropping alignments equal to the number of spanned cells
            Some((cell, (cs, as.drop(span))))
          case _ => None
        }
        dom.Table.Row(cells.toList: _*)
      }

    headRows ~ alignRow ~ bodyRows ^~ { (hd, al, bd) =>
      // Expand the alignments by the cells spanned by each alignment
      val alignments = al.flatMap({ case (a, n) => List.fill(n)(a) }).toList
      dom.Table(head = mkRows(hd, alignments), body = mkRows(bd, alignments))
    }
  }

  /* An expression which stands alone as a block, without being wrapped in a paragraph. */
  lazy val expressionBlock: Parser[dom.ExpressionBlock] = rule {
    // N.B. the block must be followed by a blank line, otherwise it is a paragraph
    sp.? ~> embeddableExpression <~ sp.? ~ ?=(blankLine) ^* dom.ExpressionBlock
  }

  lazy val blockquote: Parser[dom.Blockquote] = {
    val announcedLine = ">" ~ " ".? ~> blockLine_?
    val blockquoteBlock = announcedLine ~ (announcedLine | blockLine).* ^~ { (init, subs) => parseExtents(&(blocks), init +: subs) }

    repSep(1, blockquoteBlock, interBlock_?) ^* { p => dom.Blockquote(p.collect({ case Left(b) => b }).flatten) }
  }

  lazy val heading: Parser[dom.Heading] =
    sp.? ~> "#".*>=(1) ~ blockInlines ^~ ((h, is) => dom.Heading(h.length, is))

  lazy val paragraph: Parser[dom.Paragraph] =
    blockInlines ^* dom.Paragraph

  lazy val blockInlines: Parser[Seq[Inline]] =
    sp.? ~> (inline.+ <~ sp.?).+ ^*(_.flatten)

  lazy val blockInlines_? : Parser[Seq[Inline]] =
    sp.? ~> (inline.+ <~ sp.?).* ^*(_.flatten)

  lazy val blockLine = ?!(blankLine) ~> blockLine_?

  lazy val blockLine_? =  (?!(newLine) ~ blockAtom).* ~ newLine.? ^^(_.parsed)

  /** An indivisible element of a block; used to quickly scan block content. Matches either a single character, or
    * a 'modal' element requiring specific line handling differing from the ordinary block rules. */
  lazy val blockAtom = {
    val modal = (
      comment
    | link
    | autoLink
    | code
    | objectLiteralExpression // attributes
    | embeddableExpression
    )

    ?=(specialChar) ~ modal | unicodeCharacter
  }

  //endregion

  //region Inlines

  lazy val inline: Parser[Inline] = (
    text
  | lineBreak
  | attributes
  | space
  | strong
  | emphasis
  | quoted
  | link
  | autoLink
  | code
  | entity
  | inlineExpression
  | superscript
  | subscript
  | symbol
  )

  /** An object literal expression defining attributes of the enclosing block. */
  lazy val attributes: Parser[dom.Attributes] =
    // N.B. that a preceding space is consumed if there is a trailing space so as to avoid
    // creating multiple space nodes.
    (
      space ~> objectLiteralExpression <~ ?=(space)
    |          objectLiteralExpression
    ) ^* { expr => dom.Attributes(expr.props) }

  /** A link of the form `[link text][optional refid](url, args)`. */
  lazy val link: Parser[dom.Link] = {
    val label = "[" ~> (?!("]") ~> &(inline)).* <~ "]"

    label ~ referenceId.? ~ parenthesizedArgumentList.? ^~ { (lbl, ref, args) => dom.Link(lbl, ref, args.getOrElse(Seq())) }
  }

  /** A reference id enclosed in square brackets. */
  lazy val referenceId: Parser[dom.ReferenceId] = (
    "[" ~> ((?!(CodePoint.Values(newLineCharValues + ']')) ~ unicodeCharacter).* ^^ (_.parsed)) <~ "]"
  ) ^* { p => dom.ReferenceId(p.toString) }

  lazy val autoLink: Parser[dom.AutoLink] =
    // TODO: decoding? more forgiving?
    "<" ~ ?=(englishAlpha.+ ~ ":") ~> (iriLiteralExpression ^* { iri => dom.AutoLink(iri.value) }) <~ ">"

  lazy val subscript   = "~" ~> (?!(whitespaceChar | "~") ~> &(inline)).+ <~ "~" ^* dom.Subscript
  lazy val superscript = "^" ~> (?!(whitespaceChar | "^") ~> &(inline)).+ <~ "^" ^* dom.Superscript

  lazy val strong = "**" ~> (?!("**") ~> &(inline)).+ <~ "**" ^* dom.Strong

  lazy val emphasis = "*" ~> (?!("*") ~> &(inline) | &(strong)).+ <~ "*" ^* dom.Emphasis

  lazy val lineBreak =
    sp ~ "\\" ~ ?=(blankLine) ~ sp ^^^ dom.LineBreak()

  lazy val text: Parser[dom.Text] = {
    val apos = "'" ~ Grapheme.Category(UnicodeCategory.Groups.Letter ++ UnicodeCategory.Groups.Number)

    normalChar.+ ~ apos.* ^^ { r => dom.Text(r.parsed.toString)}
  }

  /** Any non-empty combination of comments and whitespace not leaving or at the end of a block. */
  lazy val space = sp ~ ?!(blankLine) ^^^ dom.Space()

  lazy val entity = escape ^* dom.Entity

  lazy val quoted = |<< {
    Seq(
      "\"" -> dom.Quoted.QuoteKind.Double,
      "'"  -> dom.Quoted.QuoteKind.Single
    ) map { case (qs, kind) =>
      val quot = qs.p
      quot ~> (?!(quot) ~> &(inline)).* <~ quot ^* { is => dom.Quoted(is, kind) }
    }
  }

  /** Backtick-enclosed code. */
  lazy val code = {
    val grammar = new VerbatimStringGrammar { protected def contentAtom: Parser[Any] = ?!(newLine) ~ unicodeCharacter }
    grammar ^* { s => dom.Code(s) }
  }

  /* An inline expression starting with a twirl. */
  lazy val inlineExpression = embeddableExpression ^* dom.InlineExpression

  lazy val symbol = CodePoint.Values(specialCharValues) ^* { p => dom.Symbol(p.charSequence.toString) }

  /** Any non-empty combination of comments and whitespace not consuming a blank line. */
  protected lazy val sp = {
    // Any non-empty amount of whitespace not consuming a blank line
    val ws = ( spaceChar.+ ~ (newLine ~ spaceChars_? ~ ?!(blankLine)).?
             | newLine ~ spaceChars_? ~ ?!(blankLine)
             )

    ( ws ~ (comment ~ ws.?).*
    | (comment ~ ws.?).+
    )
  }


  /** A normal character; not a special or whitespace character. */
  protected lazy val normalChar = rule { !Grapheme.SingleCodePoint(CodePoint.Values(specialCharValues ++ whitespaceCharValues)) }

  /** A special character; a character which can denote the beginning of a structural element. */
  protected lazy val specialChar = rule { CodePoint.Values(specialCharValues) }

  /** The set of special character values; characters which can denote the beginnig of a structural element. */
  protected lazy val specialCharValues = Set(
    '*',        // strong, emphasis
    '~', '^',   // subscript, superscript
    '\'', '\"', // quotes
    '`',        // ticks
    '/',        // comments
    '\\',       // escape sequence
    '[', ']',   // labels
    '<', '>',   // autolinks
    '|',        // table cell delimiter
    '{', '}',   // attributes
    '@'         // twirl
  )

  //endregion

  //region Expressions

  /* The 'twirl'; used to switch between markdown and expression mode. */
  val twirl = "@".p

  /** An expression which can be embedded in markdown following the twirl.
    * The user is restricted to either a control structure, or a left-hand-side expression.
    * This effectively prohibits the use of binary operators without explicitly adding parentheses
    * to denote the start and end of the expression.
    * A semicolon ''immediately'' following an expression is consumed to prevent the proliferation
    * of misplaced semicolons resulting from programmers adding them out of habit. */
  lazy val embeddableExpression =
    ?=(twirl) ~> ( twirl ~> conditionalExpression           // control structures
                 |          leftHandSideExpression <~ ";".? // first leaving the twirl for an identifier
                 | twirl ~> leftHandSideExpression <~ ";".? // now discarding the twirl
                 )

  lazy val expr: Parser[Expression] = rule(
    conditionalExpression
  | logicalOrExpression
  )

  lazy val conditionalExpression = {
    val `if`   = rule {                          "if"   ~ ?!(identifierExpressionPart) ~ sp.? }
    val `then` = rule { sp.? ~ "then" ~ ?!(identifierExpressionPart) ~ sp.? }
    val `else` = rule { sp.? ~ "else" ~ ?!(identifierExpressionPart) ~ sp.? }

    val cond = rule(
      parenthesizedExpression <~ (`then` | sp.?)
    | &(expr)                 <~ `then`
    )

    `if` ~> cond ~ &(expr) ~ (`else` ~> &(expr)).? ^~ { (c, t, e) => dom.Conditional(c, t, e) }
  }

  lazy val logicalOrExpression = binOp(logicalAndExpression,  "||" ~ ?!("=")       ^^^ dom.LogicalOr)

  lazy val logicalAndExpression = binOp(bitwiseOrExpression,  "&&" ~ ?!("=")       ^^^ dom.LogicalAnd)

  lazy val bitwiseOrExpression = binOp(bitwiseXOrExpression,  "|"  ~ ?!("|" | "=") ^^^ dom.BitwiseOr)

  lazy val bitwiseXOrExpression = binOp(bitwiseAndExpression, "^"  ~ ?!("=")       ^^^ dom.BitwiseXOr)

  lazy val bitwiseAndExpression = binOp(equalityExpression,   "&"  ~ ?!("&" | "=") ^^^ dom.BitwiseAnd)

  lazy val equalityExpression = binOp(relationalExpression,
    "===" ^^^ dom.StrictEquals
  | "=="  ^^^ dom.Equals
  | "!==" ^^^ dom.StrictNotEquals
  | "!="  ^^^ dom.NotEquals
  )

  lazy val relationalExpression: Parser[Expression] = binOp(shiftExpression,
    ">="         ^^^ dom.GreaterThanOrEqualTo
  | "<="         ^^^ dom.LessThanOrEqualTo
  | ">"          ^^^ dom.GreaterThan
  | "<"          ^^^ dom.LessThan
  )

  lazy val shiftExpression: Parser[Expression] = binOp(additiveExpression,
    "<<"  ^^^ dom.LeftShift
  | ">>>" ^^^ dom.UnsignedRightShift
  | ">>"  ^^^ dom.RightShift
  )

  lazy val additiveExpression: Parser[Expression] = binOp(multiplicativeExpression,
    "+" ^^^ dom.Addition
  | "-" ^^^ dom.Subtraction
  )

  lazy val multiplicativeExpression: Parser[Expression] = binOp(unaryExpression,
    "*" ^^^ dom.Multiplication
  | "/" ^^^ dom.Division
  | "%" ^^^ dom.Modulo
  )

  private def binOp[A](operand: Parser[A], ops: Parser[(A, A) => A]): Parser[A] =
    operand ~ (
      sp.? ~ ops ~ sp.? ~ operand ^* { case (_, op, _, rhs) => (op, rhs) }
    ).* ^* { case (lhs, ops) => (lhs /: ops) { (body, op) => op._1(body, op._2) } }

  lazy val unaryExpression: Parser[Expression] = (
    "!" ~ sp.? ~> &(unaryExpression) ^* dom.LogicalNot
  | "-" ~ sp.? ~> &(unaryExpression) ^* dom.Negative
  | "+" ~ sp.? ~> &(unaryExpression) ^* dom.Positive
  | "~" ~ sp.? ~> &(unaryExpression) ^* dom.BitwiseNot
  | leftHandSideExpression
  )

  lazy val leftHandSideExpression: Parser[Expression] = (
    primaryExpression ~ sp.? ~ (
      parenthesizedArgumentList    ^* { args => (body: Expression) => dom.Application(body, args) }
    | "." ~ sp.? ~> identifierName ^* { name => (body: Expression) => dom.Member(body, name)      }
    ).*
    ^* { case (body, _, builders) => (body /: builders) { (x, bld) => bld(x) } }
  )

  /** A parenthesized list of 0 or more arguments. */
  protected lazy val parenthesizedArgumentList = "(" ~ sp.? ~> argumentList.? <~ sp.? ~ ")" ^* { _.getOrElse(Seq()) }

  /** An unparenthesized list of 1 or more arguments. */
  protected lazy val argumentList = {
    val argument = (propertyName <~ sp.? ~ "=" ~ sp.?).? ~ &(expr)
    repSepR(1, argument, sp.? ~ "," ~ sp) ^* { _ map { case (n, v) => dom.Argument(n, v) } }
  }

  /** An identifier, literal, array, object, or parenthesized expression. */
  lazy val primaryExpression: Parser[Expression] = (
    identifierExpression
  | literalExpression
  | arrayLiteralExpression
  | objectLiteralExpression
  | parenthesizedExpression
  )

  protected lazy val parenthesizedExpression =  "(" ~ sp.? ~> &(expr) <~ sp.? ~ ")"

  lazy val arrayLiteralExpression: Parser[dom.ArrayLiteral] = {
    /** A non-elided array element preceded by any number of elided elements. */
    val separator = rule { sp.? ~ "," ~ sp.? }
    val subsequentArrayElement =
      separator.+ ~ &(expr) ^~ { (seps, e) => seps.tail.map(_ => None) :+ Some(e) }

    val arrayElements = ( &(expr) ~ subsequentArrayElement.* <~ separator.* ^~  { (e, ses) => Some(e) +: ses.flatten }
                        | subsequentArrayElement.+           <~ separator.* ^*  {      (p) => None +: p.flatten      }
                        | separator.*                                       ^^^ {             Seq()                  }
                        )

    "[" ~ sp.? ~> arrayElements <~ sp.? ~ "]" ^* { es => dom.ArrayLiteral(es) }
  }

  lazy val objectLiteralExpression: Parser[dom.ObjectLiteral] = {
    val property = (propertyName <~ sp.? ~ "=" ~ sp.?) ~ &(expr) ^* { case (n, v) => dom.Attribute(n, v) }
    val separator = sp.? ~ "," ~ sp.?
    val properties = repSepR(0, property, separator)

    "{" ~ sp.? ~> properties <~ sp.? ~ "}" ^* { p => dom.ObjectLiteral(p) }
  }

  protected lazy val propertyName = (
    stringLiteralExpression   ^* { _.value          }
  | verbatimLiteralExpression ^* { _.value          }
  | numericLiteralExpression  ^* { _.value.toString }
  | identifierName
  )

  // Identifiers

  lazy val identifierExpression: Parser[dom.Identifier] = twirl ~> identifierName ^* dom.Identifier

  protected lazy val identifierName: Parser[String] =
    // Not a keyword: not a keyword followed by something other than an identifier part
    ?!(keyword ~ ?!(identifierExpressionPart)) ~> identifierExpressionStart ~ identifierExpressionPart.* ^* {
      case (s, ps) => (new StringBuilder(s) /: ps.flatten) { (sb, p) => sb.append(p) }.toString
    }

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

  //region IriLiteral

  lazy val iriLiteralExpression: Parser[dom.IriLiteral] = {
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
    ) ^^ { p => dom.IriLiteral(p.parsed.toString) }
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

    /** Valid IRI characters as defined in $iriRfc, but which cannot appear at the end of an IRI dom. */
    val nonTerminalChar = GraphemeParser(Grapheme.SingleCodePoint(CodePoint.Values(',', ';', ':')))

    (
      char
    | nonTerminalChar ~ &(iriAtom)
    | "%" ~ hexDigit.*(2)
    | "(" ~ (&(iriAtom) | nonTerminalChar).* ~ ")"
    | "[" ~ (ipv6Address | ipvFutureAddress) ~ "]"
    )
  }

  //endregion

  lazy val nullLiteralExpression: Parser[dom.NullLiteral] = "null" ^^^ dom.NullLiteral()

  lazy val booleanLiteralExpression: Parser[dom.BooleanLiteral] = (
    "true"  ^^^ true
  | "false" ^^^ false
  ) ^* dom.BooleanLiteral

  lazy val numericLiteralExpression: Parser[dom.NumericLiteral] = {
    val decimalLiteral = {
      val signedInteger =         ("+" ^^^ +1d | "-" ^^^ -1d).? ~ (digit.+ ^^(_.parsed)) ^* { p => p._1.getOrElse(1d) * p._2.toString.toDouble }
      val requiredDecimalPart =   "." ~ digit.+ ^^(_.parsed.toString.toDouble)
      val optionalDecimalPart =   ("." ~ digit.* ^^(_.parsed)).?  ^* { p => p.map(_.toString.toDouble).getOrElse(0d) }
      val optionalExponentPart =  (("e" | "E") ~ signedInteger ^*(_._2)).? ^* { p => math.pow(10d, p.map(_.toString.toDouble).getOrElse(0d)) }
      val decimalIntegerLiteral = ("0" | nonZeroDigit ~ digit.*) ^^(_.parsed.toString.toDouble)

      (
        decimalIntegerLiteral ~ optionalDecimalPart ~ optionalExponentPart ^~ { (i, d, e) => (i + d) * e }
      | requiredDecimalPart ~ optionalExponentPart                         ^~ { (d, e)    => d * e       }
      )
    }

    val hexIntegerLiteral =     "0x" ~ (hexDigit.+ ^^(_.parsed)) ^* { p => java.lang.Long.parseLong(p._2.toString, 16).toDouble }

    (hexIntegerLiteral | decimalLiteral) ^* dom.NumericLiteral
  }


  //region String Literals

  lazy val verbatimLiteralExpression = {
    val grammar = new VerbatimStringGrammar { protected def contentAtom: Parser[Any] = unicodeCharacter }
    grammar ^* { s => dom.VerbatimLiteral(s) }
  }

  trait VerbatimStringGrammar extends Parser[String] {
    protected def contentAtom: Parser[Any]

    def parse(context: ParsingContext): ParsingResult[String] =
      verbatimString.parse(context)

    lazy val verbatimString = ?=("`") ~> |<< {
                                for(n <- 1 to 16) yield {
                                  val ticks = rule { new String(Array.fill(n)('`')) }
                                  val start = rule { ticks ~ ?!("`") ~ " ".? }
                                  val end   = rule { " ".? ~ ticks           }
                                  val content = (?!(end) ~ contentAtom).+ ^^ { _.parsed }
                                  start ~> content <~ end ^* { _.toString }
                                }
                              }
  }

  lazy val stringLiteralExpression: Parser[dom.StringLiteral] = {
    val stringPart = (
      escape                               ^* { _.flatMap(Character.toChars(_)) }
    | !CodePoint.Values(newLineCharValues) ^* { _.chars                         }
    )

    |<< {
      for(qs <- Seq("\"", "'")) yield {
        val quot = qs.p
        quot ~> (?!(quot) ~> stringPart).* <~ quot ^* { p => dom.StringLiteral(new String(p.flatten.toArray)) }
      }
    }
  }

  //endregion

  lazy val keyword = LiteralSetParser(
    "case", "catch", "class", "do", "else", "extends",
    "false", "finally", "for", "if", "import", "new",
    "null", "return", "super", "then", "this", "throw",
    "true", "try", "val", "var", "while"
  )

  //endregion

  //region Common Productions

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

  /** Matches any number of comments and space characters until a newline or single line comment is encountered. */
  lazy val commentLine = rule { (spaceChars_? ~ multiLineComment).* ~ (blankLine | singleLineComment) }

  /** A C-style single or multi-line comment. */
  lazy val comment = rule { singleLineComment | multiLineComment }
  /** A C-style multi-line comment. */
  protected lazy val multiLineComment  = rule { "/*" ~ (?!("*/") ~ unicodeCharacter).* ~ "*/" }
  /** A C-style single-line comment. */
  protected lazy val singleLineComment = rule { "//" ~ line_? }

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

  protected lazy val hexDigit =          (CodePoint.Values(('0' to '9') ++ ('a' to 'f') ++ ('A' to 'F'))).p
  protected lazy val digit =             CodePoint.Values('0' to '9').p
  protected lazy val nonZeroDigit =      CodePoint.Values('1' to '9').p
  protected lazy val englishAlpha =      englishLowerAlpha | englishUpperAlpha
  protected lazy val englishLowerAlpha = CodePoint.Range('a', 'z').p
  protected lazy val englishUpperAlpha = CodePoint.Range('A', 'Z').p

  /** A space character or newline sequence. */
  protected lazy val whitespaceChar = spaceChar | newLine
  protected lazy val whitespaceCharValues = spaceCharValues ++ newLineCharValues
  /** A valid newline sequence. */
  protected lazy val newLine =    "\r\n" | CodePoint.Values(newLineCharValues)
  protected lazy val newLineCharValues = Set('\n', '\r', '\u2028', '\u2029')
  /** Zero or more spaces or tabs. */
  protected lazy val spaceChars_? = spaceChar.*
  /** A space or a tab. */
  protected lazy val spaceChar =  CodePoint.Values(' ', '\t').p
  protected lazy val spaceCharValues = Set(' ', '\t')

  /** Any single unicode grapheme. */
  protected lazy val unicodeCharacter = Grapheme.Any.p

  //endregion
}
