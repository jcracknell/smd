package smd
package grammar

import smd.parsing.{GraphemeParser, LiteralSetParser, Parsers}

trait CommonProductions extends Parsers {
  type Block = markdown.Block
  type Inline = markdown.Inline
  type Expression = expression.Expression

  def doc: Parser[markdown.Document] = ???
  def expr: Parser[Expression]
  def leftHandSideExpression: Parser[Expression]
  def iriLiteralExpression: Parser[expression.IriLiteral]
  def argumentList: Parser[Seq[Expression]]

  /** An escape sequence. Yields a sequence of code points. */
  protected lazy val escape: Parser[Seq[Int]] =
    "\\" ~> (
      (characterEscape | numericEscape | namedEscape) |
      (!CodePoint.Values(newLineCharValues) ^*(_.codePoints.map(_.value))) <~ ";".?
    )

  private lazy val characterEscape = LiteralSetParser(Map(
                                         "\"" -> '\"',
                                         "\"" -> '\"',
                                         "t"  -> '\t',
                                         "n"  -> '\n',
                                         "r"  -> '\r',
                                         "\\" -> '\\',
                                         "b"  -> '\b',
                                         "f"  -> '\f',
                                         "v"  -> '\u000b'
                                       ).mapValues(c => Seq(c.toInt)))

  private lazy val namedEscape = LiteralSetParser(NamedEntity.entities.values.map(e => (e.name, e.codePoints))) <~ ";"

  private lazy val numericEscape =
    "#" ~> (digit.*(1,6) ^^ { r => Seq(Integer.parseInt(r.parsed.toString, 10)) }) <~ ";".? |
    "#".? ~ ("u" | "x") ~> (hexDigit.*(1,6) ^^ { r => Seq(Integer.parseInt(r.parsed.toString, 16)) }) <~ ";".?


  /** A single or multi-line comment. */
  lazy val comment = singleLineComment | multiLineComment
  lazy val commentStart = "//" | "/*"
  private lazy val multiLineComment =  "/*" ~ (!:("*/") ~ unicodeCharacter).* ~ "*/"
  private lazy val singleLineComment = "//" ~ line_?

  /** The (remainder) of the the current line, including the newline sequence. */
  protected lazy val line_? = (!:(newLine) ~ unicodeCharacter).* ~ newLine.?

  /** Zero or more blank lines. */
  protected lazy val blankLines_? = (spaceChars_? ~ newLine).* ~ (spaceChars_? ~ EOF).? ^^(_.parsed)

  /** Zero or more space characters followed by a newline or the end of the input.
    * This parser should _never_ be repeated. */
  protected lazy val blankLine = spaceChars_? ~ (newLine | EOF) ^^ (_.parsed)

  /** A tab or four spaces. */
  protected lazy val indent = "\t" | "    "
  /** Up to three space characters. */
  protected lazy val nonIndentSpace_? = " ".*(0,3)


  protected lazy val hexDigit =          CodePoint.Values(('0' to '9') ++ ('a' to 'f') ++ ('A' to 'F'))
  protected lazy val digit =             CodePoint.Values('0' to '9')
  protected lazy val nonZeroDigit =      CodePoint.Values('1' to '9')
  protected lazy val englishAlpha =      englishLowerAlpha | englishUpperAlpha
  protected lazy val englishLowerAlpha = CodePoint.Range('a', 'z')
  protected lazy val englishUpperAlpha = CodePoint.Range('A', 'Z')


  /** A space character or newline sequence. */
  protected lazy val whitespace = spaceChar | newLine
  protected lazy val whitespaceCharValues = spaceCharValues ++ newLineCharValues
  /** A valid newline sequence. */
  protected lazy val newLine =    "\r\n" | CodePoint.Values(newLineCharValues)
  protected lazy val newLineCharValues = Set('\n', '\r', '\u2028', '\u2029')
  protected lazy val spaceChars_? = spaceChar.*
  protected lazy val spaceChar =  CodePoint.Values(' ', '\t')
  protected lazy val spaceCharValues = Set(' ', '\t')

  /** Any single unicode grapheme. */
  protected lazy val unicodeCharacter = Grapheme.Any
}
