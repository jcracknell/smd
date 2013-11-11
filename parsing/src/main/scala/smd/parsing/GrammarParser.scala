package smd.parsing

abstract class GrammarParser[+A] extends Parser[A] {
  def parse(context: ParsingContext): ParsingResult[A] = grammar.parse(context)

  protected def grammar: Parser[A]
}
