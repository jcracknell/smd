package smd
package parsing

case class NotPredicateParser(body: Parser[Any]) extends Parser[Unit] {
  def parse(context: ParsingContext): ParsingResult[Unit] = {
    val rb = context.resultBuilder
    val r = body.parse(context.copy)
    if(r.accepted) rb.reject else rb.accept(())
  }
}
