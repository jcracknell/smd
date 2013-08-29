package smd
package grammar

import smd.parsing.Parsers

trait ExpressionProductions extends Parsers with CommonProductions {
  lazy val Keyword = "break" || "case" || "catch" || "class" || "const" || "continue" || "debugger" ||
                     "default" || "delete" || "do" || "else" || "enum" || "export" || "extends" ||
                     "false" || "finally" || "for" || "function" || "if" || "import" || "instanceof" ||
                     "in" || "new" || "null" || "return" || "super" || "switch" || "this" || "throw" ||
                     "true" || "try" || "typeof" || "var" || "void" || "while" || "with"


  lazy val HexadecimalEscapeSequence = "x" ~ HexDigit.*(2)

  lazy val UnicodeEscapeSequence = "u" ~ HexDigit.*(4)
}
