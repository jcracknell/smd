package grammar

import smd.parsing.Parsers

object ExpressionGrammar extends Parsers {
  lazy val keyword = "break" || "case" || "catch" || "class" || "const" || "continue" || "debugger" ||
                     "default" || "delete" || "do" || "else" || "enum" || "export" || "extends" ||
                     "false" || "finally" || "for" || "function" || "if" || "import" || "instanceof" ||
                     "in" || "new" || "null" || "return" || "super" || "switch" || "this" || "throw" ||
                     "true" || "try" || "typeof" || "var" || "void" || "while" || "with"

  lazy val hexadecimalEscapeSequence = "x" ~ hexDigit.*(2)

  lazy val unicodeEscapeSequence = "u" ~ hexDigit.*(4)

  lazy val hexDigit = "0" || "1" || "2" || "3" || "4" || "5" || "6" || "7" || "8" || "9" ||
                      "a" || "b" || "c" || "d" || "e" || "f" ||
                      "A" || "B" || "C" || "D" || "E" || "F"
}
