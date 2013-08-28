package smd
package parsing

trait SequenceParserN[+A <: Product] extends Parser[A] {
  def parse(context: ParsingContext): ParsingResult[A] = {
    val r = seq.parse(context)
    if(r.succeeded) Success(tuplize(r.product), r.index, r.length) else Failure
  }

  protected def seq: SequenceParser
  protected def tuplize(products: IndexedSeq[Any]): A
}

case class SequenceParser2[+T1, +T2](seq: SequenceParser) extends SequenceParserN[(T1, T2)] {
  protected def tuplize(products: IndexedSeq[Any]): (T1, T2) = (
    products(0).asInstanceOf[T1],
    products(1).asInstanceOf[T2]
  )
}

object SequenceParser2 {
  implicit def concatenationHeuristic[T1, T2, R]: ConcatenationHeuristic[SequenceParser2[T1, T2], Parser[R], SequenceParser3[T1, T2, R]] =
    ConcatenationHeuristic.create((l, r) => SequenceParser3(SequenceParser((l.seq.parsers :+ r):_*)))
}

case class SequenceParser3[+T1, +T2, +T3](seq: SequenceParser) extends SequenceParserN[(T1, T2, T3)] {
  protected def tuplize(products: IndexedSeq[Any]): (T1, T2, T3) = (
    products(0).asInstanceOf[T1],
    products(1).asInstanceOf[T2],
    products(2).asInstanceOf[T3]
  )
}

object SequenceParser3 {
  implicit object concatenationHeuristic extends ConcatenationHeuristic[SequenceParser3[_, _, _], Parser[_], SequenceParser] {
    def concat(lhs: SequenceParser3[_, _, _], rhs: Parser[_]): SequenceParser =
      SequenceParser((lhs.seq.parsers :+ rhs):_*)
  }
}
