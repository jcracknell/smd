package smd
package parsing

case class NotPredicateParser(body: Parser[Any]) extends Parser[Nothing] {
  def parse(context: ParsingContext): ParsingResult[Nothing] = {
    val rb = context.resultBuilder
    val r = body.parse(context.copy)
    if(r.succeeded) rb.failure else rb.nothing
  }
}
