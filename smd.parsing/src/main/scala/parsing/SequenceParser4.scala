package smd
package parsing

// THIS FILE IS AUTOMATICALLY GENERATED
// Generated at: 2013-09-03T03:01:05Z

/** Parses 4 expressions, providing strongly-typed results.
  * 
  * @param  p1  the 1st parser in the sequence.
  * @tparam T1  the product type of the 1st parser in the sequence.
  * @param  p2  the 2nd parser in the sequence.
  * @tparam T2  the product type of the 2nd parser in the sequence.
  * @param  p3  the 3rd parser in the sequence.
  * @tparam T3  the product type of the 3rd parser in the sequence.
  * @param  p4  the 4th parser in the sequence.
  * @tparam T4  the product type of the 4th parser in the sequence.
  */
case class SequenceParser4[+T1, +T2, +T3, +T4](
  p1: Parser[T1], p2: Parser[T2], p3: Parser[T3], p4: Parser[T4]
) extends AnyRef with Parser[(T1, T2, T3, T4)]
{
  def parse(context: ParsingContext): ParsingResult[(T1, T2, T3, T4)] = {
    val rb = context.resultBuilder

    val r1 = p1.parse(context)
    if(r1.failed) return rb.failure

    val r2 = p2.parse(context)
    if(r2.failed) return rb.failure

    val r3 = p3.parse(context)
    if(r3.failed) return rb.failure

    val r4 = p4.parse(context)
    if(r4.failed) return rb.failure

    rb.success((r1.product, r2.product, r3.product, r4.product))
  }
}

object SequenceParser4 {
  /** Implicit [[smd.parsing.SequencingHeuristic]] which describes how to combine an [[smd.parsing.SequenceParser4]]
    * and an [[smd.parsing.Parser]] into an [[smd.parsing.SequenceParser5]]. */
  implicit def sequencingHeuristic[L1, L2, L3, L4, R]: SequencingHeuristic[
    /*  left: */ SequenceParser4[L1, L2, L3, L4],
    /* right: */ Parser[R],
    /*  dest: */ SequenceParser5[L1, L2, L3, L4, R]
  ] =
    SequencingHeuristic.create((l, r) => SequenceParser5(l.p1, l.p2, l.p3, l.p4, r))

  /** Implicit [[smd.parsing.SequencingHeuristic]] which describes how to combine an [[smd.parsing.SequenceParser4]]
    * on the left-hand side with an [[smd.parsing.SequenceParser2]] to its right. */
  implicit def sequencingHeuristic_4_2[L1, L2, L3, L4, R1, R2]: SequencingHeuristic[
    /*  left: */ SequenceParser4[L1, L2, L3, L4],
    /* right: */ SequenceParser2[R1, R2],
    /*  dest: */ SequenceParser6[L1, L2, L3, L4, R1, R2]
  ] =
    SequencingHeuristic.create((l, r) => SequenceParser6(l.p1, l.p2, l.p3, l.p4, r.p1, r.p2))

  /** Implicit [[smd.parsing.SequencingHeuristic]] which describes how to combine an [[smd.parsing.SequenceParser4]]
    * on the left-hand side with an [[smd.parsing.SequenceParser3]] to its right. */
  implicit def sequencingHeuristic_4_3[L1, L2, L3, L4, R1, R2, R3]: SequencingHeuristic[
    /*  left: */ SequenceParser4[L1, L2, L3, L4],
    /* right: */ SequenceParser3[R1, R2, R3],
    /*  dest: */ SequenceParser7[L1, L2, L3, L4, R1, R2, R3]
  ] =
    SequencingHeuristic.create((l, r) => SequenceParser7(l.p1, l.p2, l.p3, l.p4, r.p1, r.p2, r.p3))

  /** Implicit [[smd.parsing.SequencingHeuristic]] which describes how to combine an [[smd.parsing.SequenceParser4]]
    * on the left-hand side with an [[smd.parsing.SequenceParser4]] to its right. */
  implicit def sequencingHeuristic_4_4[L1, L2, L3, L4, R1, R2, R3, R4]: SequencingHeuristic[
    /*  left: */ SequenceParser4[L1, L2, L3, L4],
    /* right: */ SequenceParser4[R1, R2, R3, R4],
    /*  dest: */ SequenceParser8[L1, L2, L3, L4, R1, R2, R3, R4]
  ] =
    SequencingHeuristic.create((l, r) => SequenceParser8(l.p1, l.p2, l.p3, l.p4, r.p1, r.p2, r.p3, r.p4))

  /** Implicit [[smd.parsing.SequencingHeuristic]] which describes how to combine an [[smd.parsing.SequenceParser4]]
    * on the left-hand side with an [[smd.parsing.SequenceParser5]] to its right. */
  implicit def sequencingHeuristic_4_5[L1, L2, L3, L4, R1, R2, R3, R4, R5]: SequencingHeuristic[
    /*  left: */ SequenceParser4[L1, L2, L3, L4],
    /* right: */ SequenceParser5[R1, R2, R3, R4, R5],
    /*  dest: */ SequenceParser9[L1, L2, L3, L4, R1, R2, R3, R4, R5]
  ] =
    SequencingHeuristic.create((l, r) => SequenceParser9(l.p1, l.p2, l.p3, l.p4, r.p1, r.p2, r.p3, r.p4, r.p5))

  /** Implicit [[smd.parsing.SequencingHeuristic]] which describes how to combine an [[smd.parsing.SequenceParser4]]
    * on the left-hand side with an [[smd.parsing.SequenceParser6]] to its right. */
  implicit def sequencingHeuristic_4_6[L1, L2, L3, L4, R1, R2, R3, R4, R5, R6]: SequencingHeuristic[
    /*  left: */ SequenceParser4[L1, L2, L3, L4],
    /* right: */ SequenceParser6[R1, R2, R3, R4, R5, R6],
    /*  dest: */ SequenceParser10[L1, L2, L3, L4, R1, R2, R3, R4, R5, R6]
  ] =
    SequencingHeuristic.create((l, r) => SequenceParser10(l.p1, l.p2, l.p3, l.p4, r.p1, r.p2, r.p3, r.p4, r.p5, r.p6))

  /** Implicit [[smd.parsing.SequencingHeuristic]] which describes how to combine an [[smd.parsing.SequenceParser4]]
    * on the left-hand side with an [[smd.parsing.SequenceParser7]] to its right. */
  implicit def sequencingHeuristic_4_7[L1, L2, L3, L4, R1, R2, R3, R4, R5, R6, R7]: SequencingHeuristic[
    /*  left: */ SequenceParser4[L1, L2, L3, L4],
    /* right: */ SequenceParser7[R1, R2, R3, R4, R5, R6, R7],
    /*  dest: */ SequenceParser11[L1, L2, L3, L4, R1, R2, R3, R4, R5, R6, R7]
  ] =
    SequencingHeuristic.create((l, r) => SequenceParser11(l.p1, l.p2, l.p3, l.p4, r.p1, r.p2, r.p3, r.p4, r.p5, r.p6, r.p7))

  /** Implicit [[smd.parsing.SequencingHeuristic]] which describes how to combine an [[smd.parsing.SequenceParser4]]
    * on the left-hand side with an [[smd.parsing.SequenceParser8]] to its right. */
  implicit def sequencingHeuristic_4_8[L1, L2, L3, L4, R1, R2, R3, R4, R5, R6, R7, R8]: SequencingHeuristic[
    /*  left: */ SequenceParser4[L1, L2, L3, L4],
    /* right: */ SequenceParser8[R1, R2, R3, R4, R5, R6, R7, R8],
    /*  dest: */ SequenceParser12[L1, L2, L3, L4, R1, R2, R3, R4, R5, R6, R7, R8]
  ] =
    SequencingHeuristic.create((l, r) => SequenceParser12(l.p1, l.p2, l.p3, l.p4, r.p1, r.p2, r.p3, r.p4, r.p5, r.p6, r.p7, r.p8))

  /** Implicit [[smd.parsing.SequencingHeuristic]] which describes how to combine an [[smd.parsing.SequenceParser4]]
    * on the left-hand side with an [[smd.parsing.SequenceParser9]] to its right. */
  implicit def sequencingHeuristic_4_9[L1, L2, L3, L4, R1, R2, R3, R4, R5, R6, R7, R8, R9]: SequencingHeuristic[
    /*  left: */ SequenceParser4[L1, L2, L3, L4],
    /* right: */ SequenceParser9[R1, R2, R3, R4, R5, R6, R7, R8, R9],
    /*  dest: */ SequenceParser13[L1, L2, L3, L4, R1, R2, R3, R4, R5, R6, R7, R8, R9]
  ] =
    SequencingHeuristic.create((l, r) => SequenceParser13(l.p1, l.p2, l.p3, l.p4, r.p1, r.p2, r.p3, r.p4, r.p5, r.p6, r.p7, r.p8, r.p9))

  /** Implicit [[smd.parsing.SequencingHeuristic]] which describes how to combine an [[smd.parsing.SequenceParser4]]
    * on the left-hand side with an [[smd.parsing.SequenceParser10]] to its right. */
  implicit def sequencingHeuristic_4_10[L1, L2, L3, L4, R1, R2, R3, R4, R5, R6, R7, R8, R9, R10]: SequencingHeuristic[
    /*  left: */ SequenceParser4[L1, L2, L3, L4],
    /* right: */ SequenceParser10[R1, R2, R3, R4, R5, R6, R7, R8, R9, R10],
    /*  dest: */ SequenceParser14[L1, L2, L3, L4, R1, R2, R3, R4, R5, R6, R7, R8, R9, R10]
  ] =
    SequencingHeuristic.create((l, r) => SequenceParser14(l.p1, l.p2, l.p3, l.p4, r.p1, r.p2, r.p3, r.p4, r.p5, r.p6, r.p7, r.p8, r.p9, r.p10))

  /** Implicit [[smd.parsing.SequencingHeuristic]] which describes how to combine an [[smd.parsing.SequenceParser4]]
    * on the left-hand side with an [[smd.parsing.SequenceParser11]] to its right. */
  implicit def sequencingHeuristic_4_11[L1, L2, L3, L4, R1, R2, R3, R4, R5, R6, R7, R8, R9, R10, R11]: SequencingHeuristic[
    /*  left: */ SequenceParser4[L1, L2, L3, L4],
    /* right: */ SequenceParser11[R1, R2, R3, R4, R5, R6, R7, R8, R9, R10, R11],
    /*  dest: */ SequenceParser15[L1, L2, L3, L4, R1, R2, R3, R4, R5, R6, R7, R8, R9, R10, R11]
  ] =
    SequencingHeuristic.create((l, r) => SequenceParser15(l.p1, l.p2, l.p3, l.p4, r.p1, r.p2, r.p3, r.p4, r.p5, r.p6, r.p7, r.p8, r.p9, r.p10, r.p11))

  /** Implicit [[smd.parsing.SequencingHeuristic]] which describes how to combine an [[smd.parsing.SequenceParser4]]
    * on the left-hand side with an [[smd.parsing.SequenceParser12]] to its right. */
  implicit def sequencingHeuristic_4_12[L1, L2, L3, L4, R1, R2, R3, R4, R5, R6, R7, R8, R9, R10, R11, R12]: SequencingHeuristic[
    /*  left: */ SequenceParser4[L1, L2, L3, L4],
    /* right: */ SequenceParser12[R1, R2, R3, R4, R5, R6, R7, R8, R9, R10, R11, R12],
    /*  dest: */ SequenceParser16[L1, L2, L3, L4, R1, R2, R3, R4, R5, R6, R7, R8, R9, R10, R11, R12]
  ] =
    SequencingHeuristic.create((l, r) => SequenceParser16(l.p1, l.p2, l.p3, l.p4, r.p1, r.p2, r.p3, r.p4, r.p5, r.p6, r.p7, r.p8, r.p9, r.p10, r.p11, r.p12))

}