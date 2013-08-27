package smd
package parsing

case class OptionalParser[+A](parser: Parser[A]) extends Parser[Option[A]] {
  def parse(context: ParsingContext): ParsingResult[Option[A]] = {
    val c = context.clone
    val r = parser.parse(c)

    if(r.succeeded) {
      context.assimilate(c)
      Success(Some(r.product), r.index, r.length)
    } else {
      Success(None, r.index, r.length)
    }
  }
}

