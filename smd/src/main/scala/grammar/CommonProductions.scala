package smd
package grammar

import smd.parsing.Parsers

trait CommonProductions extends Parsers {
  lazy val Digit =             CodePoint.Range('0', '9')
  lazy val NonZeroDigit =      CodePoint.Range('1', '9')
  lazy val HexDigit =          CodePoint.Values(('0' to '9') ++ ('a' to 'f') ++ ('A' to 'F'))
  lazy val EnglishAlpha =      CodePoint.Values(('a' to 'z') ++ ('A' to 'Z'))
  lazy val EnglishLowerAlpha = CodePoint.Range('a', 'z')
  lazy val EnglishUpperAlpha = CodePoint.Range('A', 'Z')

  lazy val Whitespace = SpaceChar || NewLine
  lazy val Indent =     "\t" || "    "
  lazy val NewLine =    "\n" || "\u2028" || "\u2029" || "\r" || "\r\n"
  lazy val SpaceChar =  CodePoint.Values(' ', '\t')

  lazy val UnicodeCharacter = Grapheme.Any
}
