package smd
package parsing

case class LeftParser[+A, +B](left: Parser[A], right: Parser[B]) extends Parser[A] {
  def parse(context: ParsingContext): ParsingResult[A] = {
    val rb = context.resultBuilder

    val lr = left.parse(context)
    if(lr.failed) return rb.failure

    val rr = right.parse(context)
    if(rr.succeeded) rb.success(lr.product) else rb.failure
  }
}
