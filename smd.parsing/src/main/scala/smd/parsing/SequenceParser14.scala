package smd
package parsing

// This file is automatically generated by an SBT task
// Last Generated: 2013-09-18T04:47:11Z

/** Parses 14 expressions, providing strongly-typed results.
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
  * @param  p9  the 9th parser in the sequence.
  * @tparam T9  the product type of the 9th parser in the sequence.
  * @param  p10 the 10th parser in the sequence.
  * @tparam T10 the product type of the 10th parser in the sequence.
  * @param  p11 the 11th parser in the sequence.
  * @tparam T11 the product type of the 11th parser in the sequence.
  * @param  p12 the 12th parser in the sequence.
  * @tparam T12 the product type of the 12th parser in the sequence.
  * @param  p13 the 13th parser in the sequence.
  * @tparam T13 the product type of the 13th parser in the sequence.
  * @param  p14 the 14th parser in the sequence.
  * @tparam T14 the product type of the 14th parser in the sequence.
  */
case class SequenceParser14[+T1, +T2, +T3, +T4, +T5, +T6, +T7, +T8, +T9, +T10, +T11, +T12, +T13, +T14](
  p1: Parser[T1], p2: Parser[T2], p3: Parser[T3], p4: Parser[T4], p5: Parser[T5], p6: Parser[T6], p7: Parser[T7], p8: Parser[T8], p9: Parser[T9], p10: Parser[T10], p11: Parser[T11], p12: Parser[T12], p13: Parser[T13], p14: Parser[T14]
) extends SequenceParserLike[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14)]
{
  lazy val sequence: IndexedSeq[Parser[Any]] = IndexedSeq(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14)

  def parse(context: ParsingContext): ParsingResult[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14)] = {
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

    val r9 = p9.parse(context)
    if(r9.failed) return rb.failure

    val r10 = p10.parse(context)
    if(r10.failed) return rb.failure

    val r11 = p11.parse(context)
    if(r11.failed) return rb.failure

    val r12 = p12.parse(context)
    if(r12.failed) return rb.failure

    val r13 = p13.parse(context)
    if(r13.failed) return rb.failure

    val r14 = p14.parse(context)
    if(r14.failed) return rb.failure

    rb.success((r1.product, r2.product, r3.product, r4.product, r5.product, r6.product, r7.product, r8.product, r9.product, r10.product, r11.product, r12.product, r13.product, r14.product))
  }
}