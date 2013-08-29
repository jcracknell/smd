package smd
package parsing

trait Parser[+A] {
  def parse(context: ParsingContext): ParsingResult[A]


  def ^^[B](transform: ParsingResult[A] => B): Parser[B] = TransformParser(this, transform)

  def ? : OptionalParser[A] = OptionalParser(this)

  def ||[B >: A](rhs: Parser[B]): OrderedChoiceParser[B] = OrderedChoiceParser(this, rhs)

  def ~[L >: this.type <: Parser[_], R, C <: Parser[_]](rhs: R)(implicit heuristic: SequencingHeuristic[L, R, C]): C =
    heuristic(this, rhs)

  def *                       = RepetitionParser(this, None,           None          )
  def *   (occurs: Int)       = RepetitionParser(this, Some(occurs),   Some(occurs)  )
  def *   (min: Int, max:Int) = RepetitionParser(this, Some(min),      Some(max)     )
  def *   (range: (Int, Int)) = RepetitionParser(this, Some(range._1), Some(range._2))
  def *>  (min: Int)          = RepetitionParser(this, Some(min + 1),  None          )
  def *>= (min: Int)          = RepetitionParser(this, Some(min),      None          )
  def *<  (max: Int)          = RepetitionParser(this, None,           Some(max - 1) )
  def *<= (max: Int)          = RepetitionParser(this, None,           Some(max)     )
  def +                       = RepetitionParser(this, Some(1),        None          )
}

object Parser {
  implicit def sequencingHeuristic[L, R]: SequencingHeuristic[Parser[L], Parser[R], SequenceParser2[L, R]] =
    SequencingHeuristic.create((l, r) => SequenceParser2(SequenceParser(l, r)))

  implicit def stringSequencingHeuristic[L]: SequencingHeuristic[Parser[L], String, SequenceParser2[L, String]] =
    SequencingHeuristic.create((l, r) => SequenceParser2(SequenceParser(l, LiteralParser(r))))
}
