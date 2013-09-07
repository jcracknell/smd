package smd
package grammar

import smd.parsing.Parsers

trait CommonProductions extends Parsers {
  protected type Expression = dom.Expression
  protected type MarkDown = dom.MarkDown
  protected val $ex = dom.Expression
  protected val $md = dom.MarkDown

  def Document: Parser[dom.MarkDown.Document] = ???
  def Expression: Parser[dom.Expression]

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


  lazy val HexDigit =          CodePoint.Values(('0' to '9') ++ ('a' to 'f') ++ ('A' to 'F'))
  lazy val Digit =             CodePoint.Values('0' to '9')
  lazy val NonZeroDigit =      CodePoint.Values('1' to '9')
  lazy val EnglishAlpha =      EnglishLowerAlpha | EnglishUpperAlpha
  lazy val EnglishLowerAlpha = CodePoint.Range('a', 'z')
  lazy val EnglishUpperAlpha = CodePoint.Range('A', 'Z')


  /** A space character or newline sequence. */
  lazy val Whitespace = SpaceChar | NewLine
  lazy val WhitespaceCharValues = SpaceCharValues ++ NewLineCharValues
  /** A valid newline sequence. */
  lazy val NewLine =    "\r\n" | CodePoint.Values(NewLineCharValues)
  lazy val NewLineCharValues = Set('\n', '\r', '\u2028', '\u2029')
  lazy val SpaceChars = SpaceChar.*
  lazy val SpaceChar =  CodePoint.Values(' ', '\t')
  lazy val SpaceCharValues = Set(' ', '\t')

  lazy val UnicodeCharacter = Grapheme.Any
}
