package smd
package parsing

// This file is automatically generated by an SBT task
// Last Generated: 2013-10-10T04:36:10Z

/** Parses 3 expressions, providing strongly-typed results.
  * 
  * @param  p1  the 1st parser in the sequence.
  * @tparam T1  the product type of the 1st parser in the sequence.
  * @param  p2  the 2nd parser in the sequence.
  * @tparam T2  the product type of the 2nd parser in the sequence.
  * @param  p3  the 3rd parser in the sequence.
  * @tparam T3  the product type of the 3rd parser in the sequence.
  */
case class SequenceParser3[+T1, +T2, +T3](
  p1: Parser[T1], p2: Parser[T2], p3: Parser[T3]
) extends SequenceParserLike[(T1, T2, T3)]
{ seq =>
  lazy val sequence: IndexedSeq[Parser[Any]] = IndexedSeq(p1, p2, p3)

  def parse(context: ParsingContext): ParsingResult[(T1, T2, T3)] = {
    val rb = context.resultBuilder

    val r1 = p1.parse(context)
    if(r1.rejected) return rb.reject

    val r2 = p2.parse(context)
    if(r2.rejected) return rb.reject

    val r3 = p3.parse(context)
    if(r3.rejected) return rb.reject

    rb.accept((r1.product, r2.product, r3.product))
  }
}