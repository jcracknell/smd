package smd
package parsing

case class RightParser[+A, +B](left: Parser[A], right: Parser[B]) extends Parser[B] {
  def parse(context: ParsingContext): ParsingResult[B] = {
    val rb = context.resultBuilder

    val lr = left.parse(context)
    if(lr.rejected) return rb.reject

    val rr = right.parse(context)
    if(rr.accepted) rb.accept(rr.product) else rb.reject
  }
}
