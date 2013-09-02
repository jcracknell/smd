package smd
package parsing

case class OptionalParser[+A](parser: Parser[A]) extends Parser[Option[A]] {
  def parse(context: ParsingContext): ParsingResult[Option[A]] = {
    val rb = context.resultBuilder
    val c = context.copy
    val r = parser.parse(c)

    if(r.succeeded) {
      context.advanceTo(c.index)
      rb.success(Some(r.product))
    } else {
      rb.success(None)
    }
  }
}

