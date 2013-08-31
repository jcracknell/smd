package smd
package grammar

import smd.{expressions => expr}
import smd.parsing.{OrderedChoiceParser, Parsers}

trait ExpressionProductions extends Parsers with CommonProductions {
  lazy val StringLiteral = DoubleQuotedStringLiteral | SingleQuotedStringLiteral | VerbatimStringLiteral
  lazy val VerbatimStringLiteral = &:("`") ~ OrderedChoiceParser(
                                     (16 to 1).map(n => new String(Array.fill(n)('`')))
                                     .map(ticks => ticks ~ (!:(ticks) ~ StringLiteralCharacter).* ~ ticks)
                                   ) -> { r => expr.StringLiteralExpression(r.parsed.toString) }
  lazy val DoubleQuotedStringLiteral = "\"" ~ (!:("\"") ~ StringLiteralCharacter).* ~ "\""
  lazy val SingleQuotedStringLiteral = "'" ~ (!:("'") ~ StringLiteralCharacter).* ~ "'"
  lazy val StringLiteralCharacter = ("\\" ~ (UnicodeEscapeSequence | HexadecimalEscapeSequence | NewLine | UnicodeCharacter)) |
                                    !CodePoint.Values('\n', '\r', '\u2028', '\u2029')

  lazy val Keyword = "break" | "case" | "catch" | "class" | "const" | "continue" | "debugger" |
                     "default" | "delete" | "do" | "else" | "enum" | "export" | "extends" |
                     "false" | "finally" | "for" | "function" | "if" | "import" | "instanceof" |
                     "in" | "new" | "null" | "return" | "super" | "switch" | "this" | "throw" |
                     "true" | "try" | "typeof" | "var" | "void" | "while" | "with"

  /** Zero or more space characters or comments. */
  lazy val ExpressionWhitespaceNoNewline = (SpaceChar | Comment).*

  /** Zero or more whitespace characters or comments. */
  lazy val ExpressionWhitespace = (Whitespace | Comment).*

  lazy val HexadecimalEscapeSequence = "x" ~ HexDigit.*(2)
  lazy val UnicodeEscapeSequence =     "u" ~ HexDigit.*(4)
}
