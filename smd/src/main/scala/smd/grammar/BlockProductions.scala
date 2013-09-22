package smd
package grammar

trait BlockProductions extends InlineProductions {
  protected def parse[A](parser: Parser[A], extents: Seq[CharSequence]): A = {
    // TODO: Performance
    parser.parse(extents.map(_.toString).mkString("")).product
  }

  override lazy val doc: Parser[markdown.Document] = blocks ^* markdown.Document

  lazy val blocks: Parser[Seq[Block]] =  repSep(0, block, interBlock) ^*(_._1)

  lazy val block: Parser[Block] = (
    heading
  | blockquote
  | paragraph
  )

  /** Zero or more blank lines interleaved with comments. */
  lazy val interBlock: Parser[Any] =
    repSep(0, blankLines, spaceChars ~ comment)

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
