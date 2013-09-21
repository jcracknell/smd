package smd
package grammar

trait BlockProductions extends InlineProductions {
  override lazy val doc: Parser[markdown.Document] = blocks ^* markdown.Document

  lazy val blocks: Parser[Seq[Block]] = {
    val orphanedComment = (spaceChars ~ comment).* ~ blankLine
    val interBlock = blankLine.* ~ (orphanedComment ~ blankLine.*).*

    (interBlock ~> block <~ interBlock).*
  }

  lazy val block: Parser[Block] = (
    heading
  | paragraph
  )

  lazy val heading: Parser[markdown.Heading] =
    spaceChars ~> "#".*>=(1) ~ blockInlinesReqd ^~ ((h, is) => markdown.Heading(h.length, is))

  lazy val paragraph: Parser[markdown.Paragraph] =
    blockInlinesReqd ^* markdown.Paragraph

  lazy val blockInlinesReqd =
    blockWhitespaceOrComments.? ~> (inline.+ <~ blockWhitespaceOrComments.?).+ ^*(_.flatten)

  lazy val blockInlines =
    blockWhitespaceOrComments.? ~> (inline.+ <~ blockWhitespaceOrComments.?).* ^*(_.flatten)

  lazy val blockLineReqd = !:(blankLine) ~> blockLine

  lazy val blockLine = {
    val atomic = (
      !:(specialChar) ~ unicodeCharacter
    | inlineExpression
    | comment
    | link
    | autoLink
    | code
    | unicodeCharacter
    )

    (!:(newLine) ~ atomic).* ~ newLine.?
  }
}
