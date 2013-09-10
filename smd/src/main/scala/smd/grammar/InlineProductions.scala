package smd
package grammar

import smd.parsing.OrderedChoiceParser

trait InlineProductions extends CommonProductions {
  lazy val inline: Parser[Inline] =
    text | lineBreak | space | strong | emphasis | quoted | code | entity | inlineExpression | symbol

  lazy val strong = "**" ~> (!:("**") ~> <>(inline)).+ <~ "**" ^* markdown.Strong

  lazy val emphasis = "*" ~> (!:("*") ~> <>(inline) | <>(strong)).+ <~ "*" ^* markdown.Emphasis

  lazy val lineBreak =
    blockWhitespaceOrComments ~ "\\" ~ &:(blankLine) ~ blockWhitespaceOrComments ^^^ markdown.LineBreak()

  lazy val text = normalChar.+ ^^ { r => markdown.Text(r.parsed.toString) }

  /** Any non-empty combination of comments and whitespace not leaving or at the end of a block. */
  lazy val space = blockWhitespaceOrComments ~ !:(blankLine) ^^^ markdown.Space()

  lazy val entity = escape ^* markdown.Entity

  lazy val quoted = OrderedChoiceParser(
    Seq(
      "\"" -> markdown.Quoted.QuoteKind.Double,
      "'"  -> markdown.Quoted.QuoteKind.Single
    ) map { case (quot, kind) =>
      quot ~> (!:(quot) ~> <>(inline)).* <~ quot ^* { is => markdown.Quoted(is, kind) }
    }
  )

  /** Backtick-enclosed code not leaving a block. */
  lazy val code =
    &:("`") ~> OrderedChoiceParser((1 to 16).reverse.map(n => "".padTo(n, '`')).map { ticks =>
      ticks ~> (
        (
          blockWhitespace.? ~
          (!:(whitespace | ticks) ~ unicodeCharacter ~ blockWhitespace.?).*
        ) ^^(_.parsed)
      ) <~ ticks
    }) ^* { p => markdown.Code(p.toString) }

  lazy val inlineExpression = &:("@") ~> leftHandSideExpression <~ ";".? ^* markdown.InlineExpression

  lazy val symbol = CodePoint.Values(specialCharValues) ^* { p => markdown.Symbol(p.charSequence.toString) }

  /** Any non-empty combination of comments and whitespace not consuming a blank line. */
  protected lazy val blockWhitespaceOrComments =
    blockWhitespace ~ (comment ~ blockWhitespace.?).* |
    (comment ~ blockWhitespace.?).+

  /** Any non-empty amount of whitespace not consuming a blank line. */
  protected lazy val blockWhitespace =
    spaceChar.+ ~ (newLine ~ spaceChars ~ !:(blankLine)).? |
    newLine ~ spaceChars ~ !:(blankLine)


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
}
