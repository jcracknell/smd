package smd
package parsing

trait Parser[+A] {
  def parse(context: ParsingContext): ParsingResult[A]
  def parse(input: CharSequence): ParsingResult[A] = parse(ParsingContext(input))

  def >>  [B](transform: ParsingResult[A] => B): Parser[B] = TransformParser(this, transform)

  def >>> [B](transform: A => B): Parser[B] = ProductTransformParser(this, transform)

  def >>>> [B](transform: => B): Parser[B] = TransformParser(this, (x: ParsingResult[A]) => transform)

  def ? : OptionalParser[A] = OptionalParser(this)

  def | [L >: this.type <: Parser[_], R <: Parser[_], C <: Parser[_]]
        (rhs: Implicit[R])
        (implicit heuristic: OrderedChoiceHeuristic[L, R, C]): C =
    heuristic(this, rhs)

  def ~ [L >: this.type <: Parser[_], R <: Parser[_], S <: Parser[_]]
        (rhs: Implicit[R])
        (implicit heuristic: SequencingHeuristic[L, R, S]): S =
    heuristic(this, rhs)

  /** Parse two expressions in sequence, discarding the results of the left-hand expression. */
  def ~> [B](rhs: Parser[B]): Parser[B] = RightParser(this, rhs)

  /** Parse two expressions in sequence, discarding the results of the right-hand expression. */
  def <~ [B](rhs: Parser[B]): Parser[A] = LeftParser(this, rhs)

  def *                       = RepetitionParser(this, None,           None          )
  def *   (occurs: Int)       = RepetitionParser(this, Some(occurs),   Some(occurs)  )
  def *   (min: Int, max:Int) = RepetitionParser(this, Some(min),      Some(max)     )
  def *>  (min: Int)          = RepetitionParser(this, Some(min + 1),  None          )
  def *>= (min: Int)          = RepetitionParser(this, Some(min),      None          )
  def *<  (max: Int)          = RepetitionParser(this, None,           Some(max - 1) )
  def *<= (max: Int)          = RepetitionParser(this, None,           Some(max)     )
  def +                       = RepetitionParser(this, Some(1),        None          )
}

object Parser {
  implicit def orderedChoiceHeuristic[A]: OrderedChoiceHeuristic[Parser[A], Parser[A], OrderedChoiceParser[A]] =
    OrderedChoiceHeuristic.create((l, r) => OrderedChoiceParser(l, r))

  implicit def sequencingHeuristic[L, R]: SequencingHeuristic[Parser[L], Parser[R], SequenceParser2[L, R]] =
    SequencingHeuristic.create((l, r) => SequenceParser2(l, r))
}
