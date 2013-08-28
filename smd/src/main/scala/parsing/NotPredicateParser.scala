package smd
package parsing

case class NotPredicateParser(body: Parser[Any]) extends Parser[Unit] {
  def parse(context: ParsingContext): ParsingResult[Unit] = {
    val r = body.parse(context.clone)
    if(r.succeeded) Failure else Success((), context.index, 0)
  }
}
