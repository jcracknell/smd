package smd
package parsing

case class ProductTransformParser[A, +B](body: Parser[A], transform: A => B) extends Parser[B] {
  def parse(context: ParsingContext): ParsingResult[B] = {
    val r = body.parse(context)
    if(r.succeeded) r.copy(transform(r.product)) else ParsingResult.Failure
  }
}
