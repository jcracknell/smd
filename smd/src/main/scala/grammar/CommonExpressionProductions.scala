package smd
package grammar

import smd.{expressions => expr}
import smd.parsing.{OrderedChoiceParser, Parsers}

trait CommonExpressionProductions extends Parsers with CommonProductions {
  protected lazy val ArgumentSeparator = ExpressionWhitespace ~ "," ~ ExpressionWhitespace >>>>(())

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
