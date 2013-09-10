package smd
package grammar

import smd.parsing.LiteralSetParser


trait CommonExpressionProductions extends CommonProductions {
  /** An argument list, including parentheses. */
  lazy val argumentList: Parser[Seq[Expression]] = "(" ~ expressionWhitespace ~ argumentListArguments.? ~ expressionWhitespace ~ ")" ^*(_._3.getOrElse(Seq()))

  private lazy val argumentListArguments = <>(expr) ~ (argumentSeparator ~ <>(expr)).* ^* { p => p._1 +: p._2.map(_._2) }

  protected lazy val argumentSeparator = expressionWhitespace ~ "," ~ expressionWhitespace ^^^(())

  lazy val keyword = LiteralSetParser(
                       "break", "case", "catch", "class", "const", "continue", "debugger",
                       "default", "delete", "do", "else", "enum", "export", "extends",
                       "false", "finally", "for", "function", "if", "import", "instanceof",
                       "in", "new", "null", "return", "super", "switch", "this", "throw",
                       "true", "try", "typeof", "var", "void", "while", "with"
                     )

  /** Zero or more space characters or comments. */
  lazy val expressionWhitespaceNoNewline = (spaceChar | comment).*

  /** Zero or more whitespace characters or comments. */
  lazy val expressionWhitespace = (whitespace | comment).*

}
