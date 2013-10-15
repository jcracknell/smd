package smd
package parsing

case class AndPredicateParser[+A](body: Parser[A]) extends Parser[A] {
  def parse(context: ParsingContext): ParsingResult[A] =
    body.parse(context.copy)
}
