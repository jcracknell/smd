package smd
package parsing

case class RightParser[+A, +B](left: Parser[A], right: Parser[B]) extends Parser[B] {
  def parse(context: ParsingContext): ParsingResult[B] = {
    val rb = context.resultBuilder

    val lr = left.parse(context)
    if(lr.failed) return rb.failure

    val rr = right.parse(context)
    if(rr.succeeded) rb.success(rr.product) else rb.failure
  }
}
