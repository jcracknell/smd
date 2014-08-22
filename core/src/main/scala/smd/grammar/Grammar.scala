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

  lazy val unorderedList = new ListGrammar[dom.UnorderedList] {
    type MarkerProduct = Any
    lazy val marker = unorderedListMarker

    def mkLoose(items: Seq[(MarkerProduct, Seq[Block])]): dom.UnorderedList =
      dom.LooseUnorderedList(items map { case (_, content) => dom.LooseUnorderedList.Item(content) })

    def mkTight(items: Seq[(MarkerProduct, Seq[dom.Inline], Seq[dom.List])]): dom.UnorderedList =
      dom.TightUnorderedList(items map { case (_, content, sublists) => dom.TightUnorderedList.Item(content, sublists) })
  }

  lazy val orderedList: Parser[dom.OrderedList] = {
    // TODO: is there a sane way we can enforce the same counter name across all list elements?
    ?=(orderedListMarkerLike) ~> |<< {
      orderedListMarkerVariants map { case (numeralStyle, separatorStyle, markerParser) =>
        new ListGrammar[dom.OrderedList] {
          type MarkerProduct = (Option[String], CharSequence)
          def marker = markerParser

          def mkLoose(items: Seq[(MarkerProduct, Seq[Block])]): dom.OrderedList =
            items.head match { case ((counterName, start), _) =>
              dom.LooseOrderedList(
                dom.OrderedList.Counter(numeralStyle, separatorStyle, numeralStyle.decode(start), counterName),
                items map { case (_, content) => dom.LooseOrderedList.Item(content) }
              )
            }

          def mkTight(items: Seq[(MarkerProduct, Seq[Inline], Seq[dom.List])]): dom.OrderedList =
            items.head match { case ((counterName, start), _, _) =>
              dom.TightOrderedList(
                dom.OrderedList.Counter(numeralStyle, separatorStyle, numeralStyle.decode(start), counterName),
                items map { case (_, content, sublists) => dom.TightOrderedList.Item(content, sublists) }
              )
            }
        }
      }
    }
  }

  lazy val definitionList: Parser[dom.DefinitionList] = rule {
    val term = blockLine

    val definitionMarker: Parser[Any] = nonIndentSpace_? ~ (":" | "~") ~ spaceChar.+

    val definitions = new ListGrammar[Seq[dom.DefinitionList.Definition]] {
      type MarkerProduct = Any
      def marker = definitionMarker

      def mkLoose(items: Seq[(MarkerProduct, Seq[Block])]): Seq[dom.DefinitionList.Definition] =
        items map { case (_, content) => dom.LooseDefinitionList.Definition(content) }

      def mkTight(items: Seq[(MarkerProduct, Seq[Inline], Seq[dom.List])]): Seq[dom.DefinitionList.Definition] =
        items map { case (_, content, sublists) => dom.TightDefinitionList.Definition(content, sublists) }
    }

    val itemTight = term ~ definitions.tightList
    val itemLoose = (term <~ interBlock_?) ~ definitions.looseList

    val tightList = (
      // Here we know that itemTight would not have matched the beginning of loose definitions
      itemTight.+ <~ ?!(interBlock_? ~ itemLoose)
    ) ^* { items => 
      dom.TightDefinitionList(items map { case (termExtent, definitions) => 
        var term = dom.DefinitionList.Term(parseExtents(blockInlines_?, Seq(termExtent)))
        val defs = definitions collect { case d: dom.TightDefinitionList.Definition => d }
        dom.TightDefinitionList.Item(term, defs)
      })
    }

    val looseList = repSepR(1, itemLoose, interBlock_?) ^* { items =>
      dom.LooseDefinitionList(items map { case (termExtent, definitions) =>
        var term = dom.DefinitionList.Term(parseExtents(blockInlines_?, Seq(termExtent)))
        val defs = definitions collect { case d: dom.LooseDefinitionList.Definition => d }
        dom.LooseDefinitionList.Item(term, defs)
      })
    }

    tightList | looseList
  }

  /** Generic grammar for lists. */
  abstract class ListGrammar[+L] extends Parser[L] with ListItemGrammar {
    def mkLoose(items: Seq[(MarkerProduct, Seq[dom.Block])]): L
    def mkTight(items: Seq[(MarkerProduct, Seq[dom.Inline], Seq[dom.List])]): L

    def parse(context: ParsingContext): ParsingResult[L] = list.parse(context)

    lazy val list: Parser[L] = rule {  tightList | looseList }

    // A 'tight' list is a sequence of tight list items not followed by loose content.
    lazy val tightList = itemTight.+ <~ ?!(interBlock_? ~ (continuationLine | itemLoose)) ^* { rawItems =>
      val items = rawItems map { case (m, contentExtents, sublistExtents) =>
        val content  = if(contentExtents.isEmpty) Nil else parseExtents(blockInlines_?, contentExtents)
        val sublists = if(sublistExtents.isEmpty) Nil else parseExtents((unorderedList | orderedList).*, sublistExtents)
        (m, content, sublists)
      }
      mkTight(items)
    }

    lazy val looseList = repSepR(1, itemLoose, interBlock_?) ^* { p =>
      val items = p collect { case (m, extents) => (m, parseExtents(blocks, extents)) }
      mkLoose(items)
    }
  }

  /** Generic grammar for list items. */
  trait ListItemGrammar {
    type MarkerProduct
    def marker: Parser[MarkerProduct]

    // TODO: This differs from `listMarker` only in the case of definition list markers. This behavior
    // should be refactored so it is explicit.
    protected lazy val anyMarker = marker | listMarker

    lazy val markedLine       = rule {                 marker   ~  blockLine_? }
    lazy val unmarkedLine     = rule { ?!(anyMarker) ~ indent.? ~> blockLine   }
    lazy val continuationLine = rule { ?!(anyMarker) ~ indent   ~> blockLine   }

    lazy val itemLoose: Parser[(MarkerProduct, Seq[CharSequence])] = {
      val initialBlock      = markedLine       ~ unmarkedLine.* ^* { case ((m, a), bs) => (m, a +: bs) }
      val continuationBlock = continuationLine ~ unmarkedLine.* ^* { case (a, bs) => a +: bs }

      initialBlock ~ (interBlock_? ~ continuationBlock).* ^* { case ((m, as), bs) =>
        (m, as ++ (bs flatMap { case (ib, cb) => ib +: cb }))
      }
    }

    lazy val itemTight: Parser[(MarkerProduct, Seq[CharSequence], Seq[CharSequence])] = {
      // Special handling is required for tight list items because as soon as a list marker is encountered
      // at the start of a line, the content ends and we enter a sublist.
      val content = (
        (?!(marker   ~ anyMarker) ~> markedLine  )
      ~ (?!(indent.? ~ anyMarker) ~> unmarkedLine).*
      ) ^* { case ((m, a), bs) => (m, Left(a +: bs)) }
      
      val noContent = markedLine ^* { case (m, a) => (m, Right(Seq(a))) }

      (content | noContent) ~ unmarkedLine.* ^* { case ((m, as), bs) =>
        (m, as.left.getOrElse(Nil), as.right.getOrElse(Nil) ++ bs)
      }
    }
  }

  protected lazy val listMarker = ?=(nonIndentSpace_?) ~> (
    unorderedListMarker
  | (?=(orderedListMarkerLike) ~> |<<(orderedListMarkerVariants map { case (_, _, p) => p }))
  )

  protected lazy val unorderedListMarker = nonIndentSpace_? ~ ("*" | "-" | "+") ~ spaceChar.+

  protected lazy val orderedListMarkerVariants: Seq[(dom.OrderedList.NumeralStyle, dom.OrderedList.SeparatorStyle, Parser[(Option[String], CharSequence)])] = {
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

    for {
      (numeralStyle, numeral)               <- numeralStyles
      (separatorStyle, sepBefore, sepAfter) <- separatorStyles
    } yield (
      numeralStyle,
      separatorStyle,
      nonIndentSpace_? ~ sepBefore ~> counterName_? ~ ("#" | numeral ^^ (_.parsed)) <~ sepAfter ~ spaceChar.+
    ) 
  }

  // For performance reasons we use a regex to quickly check if there is a valid counter ahead
  // This is also a relatively concise and comprehensible reference for the counter grammar.
  protected lazy val orderedListMarkerLike = 
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

    val alignRow = +> ~> (align       ~  <+>).* ~ align       ~ <+ ^~ { (as, fa, fs) => as :+ ((fa, fs)) }
    val row      = |> ~> (cellContent ~  <|>).* ~ cellContent ~ <| ^~ { (cs, fc, fs) => cs :+ ((fc, fs)) }

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
    | objectLiteral // attributes
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
      space ~> objectLiteral <~ ?=(space)
    |          objectLiteral
    ) ^* { atts => dom.Attributes(atts map { case (n, v) => dom.Attributes.Attribute(n, v) }) }

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

  lazy val logicalOrExpression = binOp(logicalAndExpression,  
    "||" ~ ?!("=")                      ^^^ dom.LogicalOr
  | "|"  ~ ?!("=")                      ^^^ dom.LogicalOr
  | "or" ~ ?!(identifierExpressionPart) ^^^ dom.LogicalOr
  )

  lazy val logicalAndExpression = binOp(equalityExpression,
    "&&"  ~ ?!("=")                      ^^^ dom.LogicalAnd
  | "&"   ~ ?!("=")                      ^^^ dom.LogicalAnd
  | "and" ~ ?!(identifierExpressionPart) ^^^ dom.LogicalAnd
  )

  lazy val equalityExpression = binOp(relationalExpression,
    "===" ^^^ dom.StrictEquals
  | "=="  ^^^ dom.Equals
  | "!==" ^^^ dom.StrictNotEquals
  | "!="  ^^^ dom.NotEquals
  )

  lazy val relationalExpression: Parser[Expression] = binOp(additiveExpression,
    ">="  ^^^ dom.GreaterThanOrEqualTo
  | "<="  ^^^ dom.LessThanOrEqualTo
  | ">"   ^^^ dom.GreaterThan
  | "<"   ^^^ dom.LessThan
  )

  lazy val additiveExpression: Parser[Expression] = binOp(multiplicativeExpression,
    "+"   ^^^ dom.Addition
  | "-"   ^^^ dom.Subtraction
  )

  lazy val multiplicativeExpression: Parser[Expression] = binOp(unaryExpression,
    "*"   ^^^ dom.Multiplication
  | "/"   ^^^ dom.Division
  | "%"   ^^^ dom.Modulo
  )

  private def binOp[A](operand: Parser[A], ops: Parser[(A, A) => A]): Parser[A] =
    operand ~ (
      sp.? ~ ops ~ sp.? ~ operand ^* { case (_, op, _, rhs) => (op, rhs) }
    ).* ^* { case (lhs, ops) => (lhs /: ops) { (body, op) => op._1(body, op._2) } }

  lazy val unaryExpression: Parser[Expression] = (
    "!"                                  ~ sp.? ~> &(unaryExpression) ^* dom.LogicalNot
  | "not" ~ ?!(identifierExpressionPart) ~ sp.? ~> &(unaryExpression) ^* dom.LogicalNot
  | "-"                                  ~ sp.? ~> &(unaryExpression) ^* dom.Negative
  | "+"                                  ~ sp.? ~> &(unaryExpression) ^* dom.Positive
  | exponentiationExpression
  )

  lazy val exponentiationExpression: Parser[Expression] = binOp(leftHandSideExpression,
    "^" ^^^ dom.Exponentiation
  )

  lazy val leftHandSideExpression: Parser[Expression] = {
    val rightSide = (
      parenthesizedArgumentList    ^* { args => (body: Expression) => dom.Application(body, args) }
    | "." ~ sp.? ~> identifierName ^* { name => (body: Expression) => dom.Member(body, name)      }
    )

    primaryExpression ~ (sp.? ~> rightSide).* ^* { case (body, builders) => (body /: builders) { (x, bld) => bld(x) } }
  }

  /** A parenthesized list of 0 or more arguments. */
  protected lazy val parenthesizedArgumentList = "(" ~ sp.? ~> argumentList.? <~ sp.? ~ ")" ^* { _.getOrElse(Seq()) }


  /** An unparenthesized list of 1 or more arguments. */
  lazy val argumentList = {
    lazy val argument = (
      propertyLabel ~ &(expr) ^* { case (n, e) => (Some(n), e) }
    | &(expr) ^* { e => (None, e) }
    )

    repSepR(1, argument, argumentSeparator) ^* { _ map { case (n, v) => dom.Argument(n, v) } }
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
    val subsequentElement = argumentSeparator.+ ~ &(expr) ^~ { (seps, e) => seps.tail.map(_ => None) :+ Some(e) }

    val arrayElements = (
      &(expr) ~ subsequentElement.* <~ argumentSeparator.* ^~  { (e, ses) => Some(e) +: ses.flatten }
    | subsequentElement.+           <~ argumentSeparator.* ^*  {      (p) => None +: p.flatten      }
    | argumentSeparator.*                                  ^^^ {             Seq()                  }
    )

    "[" ~ sp.? ~> arrayElements <~ sp.? ~ "]" ^* { es => dom.ArrayLiteral(es) }
  }

  lazy val objectLiteralExpression: Parser[dom.ObjectLiteral] =
    objectLiteral ^* { attrs => dom.ObjectLiteral(attrs map { case (n, v) => dom.ObjectLiteral.Property(n, v) }) }

  lazy val objectLiteral: Parser[Seq[(String, dom.Expression)]] =
    "{" ~ sp.? ~> repSepR(0, propertyLabel ~ &(expr), argumentSeparator) <~ sp.? ~ "}"

  protected lazy val propertyLabel: Parser[String] = {
    val propertyName = (
      stringLiteralExpression   ^* { _.value          }
    | verbatimLiteralExpression ^* { _.value          }
    | numericLiteralExpression  ^* { _.value.toString }
    | identifierName
    )

    propertyName <~ sp.? ~ "=" ~ sp.?
  }

  /** An argument separator - at least one space with an optional comma. */
  lazy val argumentSeparator: Parser[Any] = ("," ~ sp.?) | (sp ~ ("," ~ sp.?).?)

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
      /*       the unary operators: */ '+', '-', '!'
    )))

    (
      ?!(
        keyword ~ ?!(iriAtom)
      | commentStart
      )
    ~ ?!(illegalStart) ~> iriAtom.+
    ) ^* { as => dom.IriLiteral(as.mkString) }
  }

  /** An indivisible portion of an IRI literal. */
  protected val iriAtom: Parser[CharSequence] = {
    val ipv4Octet = """(25[0-5]|2[0-4][0-9]|1[0-9]{2}|[1-9]{2}|[0-9])"""
    val ipv4Address = s"""${ipv4Octet}(.${ipv4Octet}){3}""".r.p

    val ipv6Address = {
      val h16: Parser[Any]  = """(?i)[0-9a-f]{1,4}""".r.p
      val ch16: Parser[Any] = """(?i):[0-9a-f]{1,4}""".r.p
      val ls32: Parser[Any] = """(?i)[0-9a-f]{1,4}:[0-9a-f]{1,4}""".r.p | ipv4Address

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

    val ipvFutureAddress = """(?i)v[0-9a-f]\.[a-z0-9-._~!$&'()*+,;=:]+""".r.p

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
      escape                                       ^* { cps => new String(cps.flatMap(Character.toChars(_)).toArray) }
    | char                                         ^^ { _.parsed }
    | nonTerminalChar ~ &(iriAtom)                 ^* { case (g, a) => new smd.util.CompositeCharSequence(g.charSequence, a) }
    | "%" ~ hexDigit.*(2)                          ^^ { _.parsed }
    | "(" ~ (&(iriAtom) | nonTerminalChar).* ~ ")" ^^ { _.parsed }
    | "[" ~ (ipv6Address | ipvFutureAddress) ~ "]" ^^ { _.parsed }
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
    "and",     "case",  "catch",   "class", "do",     "else",
    "extends", "false", "finally", "for",   "if",     "import",
    "new",     "not",   "null",    "or",    "return", "super",
    "then",    "this",  "throw",   "true",  "try",    "val",
    "var",     "while"
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
