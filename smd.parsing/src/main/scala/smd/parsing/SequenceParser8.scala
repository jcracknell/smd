package smd
package parsing

// This file is automatically generated by an SBT task
// Last Generated: 2013-09-19T01:43:05Z

/** Parses 8 expressions, providing strongly-typed results.
  * 
  * @param  p1  the 1st parser in the sequence.
  * @tparam T1  the product type of the 1st parser in the sequence.
  * @param  p2  the 2nd parser in the sequence.
  * @tparam T2  the product type of the 2nd parser in the sequence.
  * @param  p3  the 3rd parser in the sequence.
  * @tparam T3  the product type of the 3rd parser in the sequence.
  * @param  p4  the 4th parser in the sequence.
  * @tparam T4  the product type of the 4th parser in the sequence.
  * @param  p5  the 5th parser in the sequence.
  * @tparam T5  the product type of the 5th parser in the sequence.
  * @param  p6  the 6th parser in the sequence.
  * @tparam T6  the product type of the 6th parser in the sequence.
  * @param  p7  the 7th parser in the sequence.
  * @tparam T7  the product type of the 7th parser in the sequence.
  * @param  p8  the 8th parser in the sequence.
  * @tparam T8  the product type of the 8th parser in the sequence.
  */
case class SequenceParser8[+T1, +T2, +T3, +T4, +T5, +T6, +T7, +T8](
  p1: Parser[T1], p2: Parser[T2], p3: Parser[T3], p4: Parser[T4], p5: Parser[T5], p6: Parser[T6], p7: Parser[T7], p8: Parser[T8]
) extends SequenceParserLike[(T1, T2, T3, T4, T5, T6, T7, T8)]
{ seq =>
  lazy val sequence: IndexedSeq[Parser[Any]] = IndexedSeq(p1, p2, p3, p4, p5, p6, p7, p8)

  def parse(context: ParsingContext): ParsingResult[(T1, T2, T3, T4, T5, T6, T7, T8)] = {
    val rb = context.resultBuilder

    val r1 = p1.parse(context)
    if(r1.failed) return rb.failure

    val r2 = p2.parse(context)
    if(r2.failed) return rb.failure

    val r3 = p3.parse(context)
    if(r3.failed) return rb.failure

    val r4 = p4.parse(context)
    if(r4.failed) return rb.failure

    val r5 = p5.parse(context)
    if(r5.failed) return rb.failure

    val r6 = p6.parse(context)
    if(r6.failed) return rb.failure

    val r7 = p7.parse(context)
    if(r7.failed) return rb.failure

    val r8 = p8.parse(context)
    if(r8.failed) return rb.failure

    rb.success((r1.product, r2.product, r3.product, r4.product, r5.product, r6.product, r7.product, r8.product))
  }

  /** Apply a transformation to the products of this [[smd.parsing.SequenceParser8]] if parsing is successful.
    *
    * @param transform the transformation to be applied to the products of successful parsing attempts.
    * @tparam B the transformed product type.
    */
  def ^~ [B](transform: (T1, T2, T3, T4, T5, T6, T7, T8) => B): Parser[B] = new Parser[B] {
    def parse(context: ParsingContext): ParsingResult[B] = {
      val r = seq.parse(context)
      if(r.succeeded) r.copy(transform.tupled(r.product)) else ParsingResult.Failure
    }
  }
}