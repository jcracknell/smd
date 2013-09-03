package smd
package grammar


trait CommonExpressionProductions extends CommonProductions {
  /** An argument list, including parentheses. */
  lazy val ArgumentList: Parser[Seq[Expression]] = "(" ~ ExpressionWhitespace ~ ArgumentListArguments.? ~ ExpressionWhitespace ~ ")" >>>(_._3.getOrElse(Seq()))

  private lazy val ArgumentListArguments = <>(Expression) ~ (ArgumentSeparator ~ <>(Expression)).* >>> { p => p._1 +: p._2.map(_._2) }

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
