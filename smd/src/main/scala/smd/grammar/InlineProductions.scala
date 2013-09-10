package smd
package grammar

import smd.parsing.OrderedChoiceParser

trait InlineProductions extends CommonProductions {
  lazy val Inline: Parser[Inline] =
    Text | LineBreak | Space | Strong | Emphasis | Quoted | Code | Entity | InlineExpression | Symbol

  lazy val Strong = "**" ~> (!:("**") ~> <>(Inline)).+ <~ "**" ^* markdown.Strong

  lazy val Emphasis = "*" ~> (!:("*") ~> <>(Inline) | <>(Strong)).+ <~ "*" ^* markdown.Emphasis

  lazy val LineBreak =
    BlockWhitespaceOrComments ~ "\\" ~ &:(BlankLine) ~ BlockWhitespaceOrComments ^^^ markdown.LineBreak()

  lazy val Text = NormalChar.+ ^^ { r => markdown.Text(r.parsed.toString) }

  /** Any non-empty combination of comments and whitespace not leaving or at the end of a block. */
  lazy val Space = BlockWhitespaceOrComments ~ !:(BlankLine) ^^^ markdown.Space()

  lazy val Entity = Escape ^* markdown.Entity

  lazy val Quoted = OrderedChoiceParser(
    Seq(
      "\"" -> markdown.Quoted.QuoteKind.Double,
      "'"  -> markdown.Quoted.QuoteKind.Single
    ) map { case (quot, kind) =>
      quot ~> (!:(quot) ~> <>(Inline)).* <~ quot ^* { is => markdown.Quoted(is, kind) }
    }
  )

  /** Backtick-enclosed code not leaving a block. */
  lazy val Code =
    &:("`") ~> OrderedChoiceParser((1 to 16).reverse.map(n => "".padTo(n, '`')).map { ticks =>
      ticks ~> (
        (
          BlockWhitespace.? ~
          (!:(Whitespace | ticks) ~ UnicodeCharacter ~ BlockWhitespace.?).*
        ) ^^(_.parsed)
      ) <~ ticks
    }) ^* { p => markdown.Code(p.toString) }

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
