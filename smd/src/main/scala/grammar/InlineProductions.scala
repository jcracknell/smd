package smd
package grammar

trait InlineProductions extends CommonProductions {
  def Inline: Parser[$md.Inline] =
    Text | LineBreak | Space | Strong | Emphasis

  lazy val Strong = "**" ~> (!:("**") ~> <>(Inline)).+ <~ "**" ^* $md.Strong

  lazy val Emphasis = "*" ~> (!:("*") ~> <>(Inline) | <>(Strong)).+ <~ "*" ^* $md.Emphasis

  lazy val LineBreak =
    BlockWhitespaceOrComments ~ "\\" ~ &:(BlankLine) ~ BlockWhitespaceOrComments ^^^ $md.LineBreak()

  lazy val Text = NormalChar.+ ^^ { r => $md.Text(r.parsed.toString) }

  /** Any non-empty combination of comments and whitespace not leaving or at the end of a block. */
  lazy val Space = BlockWhitespaceOrComments ~ !:(BlankLine) ^^^ $md.Space()

  /** Any non-empty combination of comments and whitespace not consuming a blank line. */
  protected lazy val BlockWhitespaceOrComments =
    BlockWhitespace ~ (Comment ~ BlockWhitespace.?).* |
    (Comment ~ BlockWhitespace.?).+

  /** Any non-empty amount of whitespace not consuming a blank line. */
  protected lazy val BlockWhitespace =
    SpaceChar.+ ~ (NewLine ~ SpaceChars ~ !:(BlankLine)).? |
    NewLine ~ SpaceChars ~ !:(BlankLine)


  protected lazy val NormalChar = !Grapheme.SingleCodePoint(CodePoint.Values(SpecialCharValues ++ WhitespaceCharValues))

  protected lazy val SpecialChar = CodePoint.Values(SpecialCharValues)
  protected lazy val SpecialCharValues = Set(
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
}
