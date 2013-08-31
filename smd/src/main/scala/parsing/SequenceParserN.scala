package smd
package parsing

trait SequenceParserN[+A <: Product] extends Parser[A] {
  def parse(context: ParsingContext): ParsingResult[A] = {
    val r = seq.parse(context)
    if(r.succeeded) r.copy(tuplize(r.product)) else ParsingResult.Failure
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
  implicit def sequencingHeuristic[T1, T2, R]: SequencingHeuristic[SequenceParser2[T1, T2], Parser[R], SequenceParser3[T1, T2, R]] =
    SequencingHeuristic.create((l, r) => SequenceParser3(SequenceParser(l.seq.sequence :+ r)))
}

case class SequenceParser3[+T1, +T2, +T3](seq: SequenceParser) extends SequenceParserN[(T1, T2, T3)] {
  protected def tuplize(products: IndexedSeq[Any]): (T1, T2, T3) = (
    products(0).asInstanceOf[T1],
    products(1).asInstanceOf[T2],
    products(2).asInstanceOf[T3]
  )
}

object SequenceParser3 {
  implicit def sequencingHeuristic[T1, T2, T3, R]: SequencingHeuristic[
    SequenceParser3[T1, T2, T3],
    Parser[R],
    SequenceParser4[T1, T2, T3, R]
  ] =
    SequencingHeuristic.create((l, r) => SequenceParser4(SequenceParser(l.seq.sequence :+ r)))
}

case class SequenceParser4[+T1, +T2, +T3, +T4](seq: SequenceParser) extends SequenceParserN[(T1, T2, T3, T4)] {
  protected def tuplize(products: IndexedSeq[Any]): (T1, T2, T3, T4) = (
    products(0).asInstanceOf[T1],
    products(1).asInstanceOf[T2],
    products(2).asInstanceOf[T3],
    products(3).asInstanceOf[T4]
  )
}

object SequenceParser4 {
  implicit val sequencingHeuristic: SequencingHeuristic[SequenceParser4[_, _, _, _], Parser[_], SequenceParser] =
    SequencingHeuristic.create((l, r) => SequenceParser(l.seq.sequence :+ r))
}

