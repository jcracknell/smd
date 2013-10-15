package smd
package parsing

case class LeftParser[+A, +B](left: Parser[A], right: Parser[B]) extends Parser[A] {
  def parse(context: ParsingContext): ParsingResult[A] = {
    val rb = context.resultBuilder

    val lr = left.parse(context)
    if(lr.rejected) return rb.reject

    val rr = right.parse(context)
    if(rr.accepted) rb.accept(lr.product) else rb.reject
  }
}
