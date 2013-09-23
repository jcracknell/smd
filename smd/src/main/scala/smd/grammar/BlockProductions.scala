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
  | paragraph
  )

  /** Zero or more blank lines interleaved with comments. */
  lazy val interBlock =
    repSep(1, blankLines, spaceChars ~ comment) ^^(_.parsed)

  lazy val unorderedList: Parser[markdown.UnorderedList] = {
    /** A marker must be followed by at least one space to avoid confusion with emphasis, negative numbers, etc. */
    val marker = nonIndentSpace ~ ("*" | "-" | "+") ~ spaceChar.+

    /** A line beginning with a marker. */
    val markerLine = marker ~> blockLine
    /** A line not beginning with a marker. One leading indent is discarded if it exists. */
    val noMarkerLine = !:(marker) ~ indent.? ~> blockLineReqd

    /** A 'tight' list item is a list item which contains only a single block. */
    val itemTight = markerLine ~ noMarkerLine.* ^~ { (a, b) => a +: b }

    /** A list item continues if it is followed by interblock content and an indented line.*/
    val itemContinuesLoose = interBlock ~ &:(indent) ~ noMarkerLine.+ ^~ { (ib, _, ls) => ib +: ls }
    val itemLoose = itemTight ~ itemContinuesLoose.* ^~ { (a, b) => a ++ b.flatten }

    /** At least one tight list items not followed by valid loose content. */
    val tight: Parser[markdown.UnorderedList.Tight] = (
      itemTight.+ <~ !:(itemContinuesLoose | interBlock ~ &:(marker))
      ^* { items => markdown.UnorderedList.Tight(
                      items.map(lines => parse(blockInlines, lines))
                      .map(is => markdown.UnorderedList.Item(is))
                    )
         }
    )

    val loose: Parser[markdown.UnorderedList.Loose] = (
      repSep(1, itemLoose, interBlock.?)
      ^* { case (items, _) => markdown.UnorderedList.Loose(
                                items.map(parse(blocks, _)).map(bs => markdown.UnorderedList.Item(bs))
                              )
         }
    )

    /** Tight must be attempted first here, as it as defined as 'a list which is not loose'. */
    tight | loose
  }

  lazy val blockquote: Parser[markdown.Blockquote] = {
    val announcedLine = ">" ~ " ".? ~> blockLine
    val blockquoteBlock = announcedLine ~ (announcedLine | blockLineReqd).* ^~ { (init, subs) => parse(<>(blocks), init +: subs) }

    repSep(1, blockquoteBlock, interBlock) ^* { case (bs, _) => markdown.Blockquote(bs.flatten) }
  }

  lazy val heading: Parser[markdown.Heading] =
    spaceChars ~> "#".*>=(1) ~ blockInlinesReqd ^~ ((h, is) => markdown.Heading(h.length, is))

  lazy val paragraph: Parser[markdown.Paragraph] =
    blockInlinesReqd ^* markdown.Paragraph

  lazy val blockInlinesReqd =
    blockWhitespaceOrComments.? ~> (inline.+ <~ blockWhitespaceOrComments.?).+ ^*(_.flatten)

  lazy val blockInlines =
    blockWhitespaceOrComments.? ~> (inline.+ <~ blockWhitespaceOrComments.?).* ^*(_.flatten)

  lazy val blockLineReqd = !:(blankLine) ~> blockLine

  lazy val blockLine: Parser[CharSequence] = {
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
