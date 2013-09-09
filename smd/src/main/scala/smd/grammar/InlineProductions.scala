package smd
package grammar

trait InlineProductions extends CommonProductions {
  def Inline: Parser[Inline] =
    Text | LineBreak | Space | Strong | Emphasis | Entity | InlineExpression | Symbol

  lazy val Strong = "**" ~> (!:("**") ~> <>(Inline)).+ <~ "**" ^* markdown.Strong

  lazy val Emphasis = "*" ~> (!:("*") ~> <>(Inline) | <>(Strong)).+ <~ "*" ^* markdown.Emphasis

  lazy val LineBreak =
    BlockWhitespaceOrComments ~ "\\" ~ &:(BlankLine) ~ BlockWhitespaceOrComments ^^^ markdown.LineBreak()

  lazy val Text = NormalChar.+ ^^ { r => markdown.Text(r.parsed.toString) }

  /** Any non-empty combination of comments and whitespace not leaving or at the end of a block. */
  lazy val Space = BlockWhitespaceOrComments ~ !:(BlankLine) ^^^ markdown.Space()

  lazy val Entity = Escape ^* markdown.Entity

  lazy val InlineExpression = &:("@") ~> LeftHandSideExpression <~ ";".? ^* markdown.InlineExpression

  lazy val Symbol = CodePoint.Values(SpecialCharValues) ^* { p => markdown.Symbol(p.charSequence.toString) }

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
