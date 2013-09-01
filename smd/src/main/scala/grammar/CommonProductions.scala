package smd
package grammar

import smd.parsing.{Parser, Parsers}

trait CommonProductions extends Parsers {
  def Document: Parser[smd.nodes.DocumentNode]
  def Expression: Parser[smd.expressions.Expression]

  /** A single or multi-line comment. */
  lazy val Comment = SingleLineComment | MultiLineComment
  lazy val MultiLineComment =  "/*" ~ (!:("*/") ~ UnicodeCharacter).* ~ "*/"
  lazy val SingleLineComment = "//" ~ Line

  /** The (remainder) of the the current line, including the newline sequence. */
  lazy val Line = (!:(NewLine) ~ UnicodeCharacter).* ~ NewLine.?

  lazy val BlankLines = (SpaceChars ~ NewLine).* ~ (SpaceChars ~ EOF).?
  /** Zero or more space characters followed by a newline or the end of the input. */
  lazy val BlankLine = SpaceChars ~ (NewLine | EOF)

  /** A tab or four spaces. */
  lazy val Indent = "\t" | "    "
  /** Up to three space characters. */
  lazy val NonIndentSpace = " ".*(0,3)


  lazy val HexDigit =          Digit ++ ('a' to 'f') ++ ('A' to 'F')
  lazy val Digit =             '0' to '9'
  lazy val NonZeroDigit =      '1' to '9'
  lazy val EnglishAlpha =      EnglishLowerAlpha ++ EnglishUpperAlpha
  lazy val EnglishLowerAlpha = 'a' to 'z'
  lazy val EnglishUpperAlpha = 'A' to 'Z'

  /** A space character or newline sequence. */
  lazy val Whitespace = SpaceChar | NewLine
  /** A valid newline sequence. */
  lazy val NewLine =    "\n" | "\r" | "\r\n" | "\u2028" | "\u2029"
  lazy val SpaceChars = SpaceChar.*
  lazy val SpaceChar =  CodePoint.Values(' ', '\t')

  lazy val UnicodeCharacter = Grapheme.Any
}
