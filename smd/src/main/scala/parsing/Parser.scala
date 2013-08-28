package smd
package parsing

trait Parser[+A] {
  def parse(context: ParsingContext): ParsingResult[A]

  def ^^[B](transform: ParsingResult[A] => B): Parser[B] = TransformParser(this, transform)

  def ? : OptionalParser[A] = OptionalParser(this)

  def ~[L >: this.type <: Parser[_], R <: Parser[_], C <: Parser[_]](rhs: R)(implicit ch: ConcatenationHeuristic[L, R, C]): C =
    ch.concat(this, rhs)

  // This is necessary to save the String => LiteralParser implicit conversion for the LHS
  def ~[L >: this.type <: Parser[_], C <: Parser[_]](str: String)(implicit ch: ConcatenationHeuristic[L, LiteralParser, C]): C =
    ch.concat(this, LiteralParser(str))

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
  implicit def concatenationHeuristic[L, R]: ConcatenationHeuristic[Parser[L], Parser[R], SequenceParser2[L, R]] =
    new ConcatenationHeuristic[Parser[L], Parser[R], SequenceParser2[L, R]] {
      def concat(lhs: Parser[L], rhs: Parser[R]): SequenceParser2[L, R] =
        SequenceParser2[L, R](SequenceParser(lhs, rhs))
    }
}
