package smd
package grammar

trait BlockProductions extends InlineProductions {
  protected def parse[A](parser: Parser[A], extents: Seq[CharSequence]): A = {
    // TODO: Performance
    parser.parse(extents.map(_.toString).mkString("")).product
  }

  override lazy val doc: Parser[markdown.Document] = blocks ^* markdown.Document

  lazy val blocks: Parser[Seq[Block]] =  interBlock.? ~> repSep(0, block, interBlock.?) <~ interBlock.? ^*(_._1)

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
      repSep(1, leftHandSideExpression, separator) ^* { case (args, _) => args }
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
    val unmarkedLine = !:(marker) ~ indent.? ~> blockLine

    /** The initial block of a list item, beginning with a marker. */
    val itemInitialBlock = markedLine ~ unmarkedLine.* ^~ { (m, a, b) => (m, a +: b) }
    /** A subsequent block of a list item, indented and not starting with a list marker.
      * The indent is consumed by noMarkerLine. */
    val itemSubsequentBlock = interBlock ~ &:(indent) ~ unmarkedLine.+ ^~ { (a, _, b) => a +: b }

    /** A 'tight' list item is a list item which contains only a single block. */
    val itemTight = itemInitialBlock
    /** A 'loose' list item can contain subsequent blocks of content. */
    val itemLoose = itemInitialBlock ~ itemSubsequentBlock.* ^* { case ((m, a), bs) => (m, a ++ bs.flatten) }

    /** A 'tight' list is a sequence of tight list items not followed by loose content. */
    val tightList = (
      itemTight.+ <~ !:(interBlock ~ (marker | indent))
      ^* { items => tight(items map { case (m, ls) => (m, parse(blockInlines_?, ls)) }) }
    )

    val looseList = (
      repSep(1, itemLoose, interBlock.?)
      ^* { case (items, _) => loose(items map { case (m, ls) => (m, parse(blocks, ls)) }) }
    )

    tightList | looseList
  }

  lazy val blockquote: Parser[markdown.Blockquote] = {
    val announcedLine = ">" ~ " ".? ~> blockLine_?
    val blockquoteBlock = announcedLine ~ (announcedLine | blockLine).* ^~ { (init, subs) => parse(<>(blocks), init +: subs) }

    repSep(1, blockquoteBlock, interBlock) ^* { case (bs, _) => markdown.Blockquote(bs.flatten) }
  }

  lazy val heading: Parser[markdown.Heading] =
    spaceChars_? ~> "#".*>=(1) ~ blockInlines ^~ ((h, is) => markdown.Heading(h.length, is))

  lazy val paragraph: Parser[markdown.Paragraph] =
    blockInlines ^* markdown.Paragraph

  lazy val blockInlines: Parser[Seq[Inline]] =
    blockWhitespaceOrComments.? ~> (inline.+ <~ blockWhitespaceOrComments.?).+ ^*(_.flatten)

  lazy val blockInlines_? : Parser[Seq[Inline]] =
    blockWhitespaceOrComments.? ~> (inline.+ <~ blockWhitespaceOrComments.?).* ^*(_.flatten)

  lazy val blockLine = !:(blankLine) ~> blockLine_?

  lazy val blockLine_? : Parser[CharSequence] = {
    val atomic = (
      !:(specialChar) ~ unicodeCharacter
    | inlineExpression
    | comment
    | link
    | autoLink
    | code
    | unicodeCharacter
    )

    (!:(newLine) ~ atomic).* ~ newLine.? ^^(_.parsed)
  }
}
