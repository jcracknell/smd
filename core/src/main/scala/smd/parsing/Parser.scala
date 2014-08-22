package smd
package parsing

import smd.util.CompositeCharSequence

abstract class Parser[+A] { lhs =>
  def parse(context: ParsingContext): ParsingResult[A]
  def parse(input: CharSequence): ParsingResult[A] = parse(ParsingContext(input))
  def parse(inputParts: Seq[CharSequence]): ParsingResult[A] = parse(CompositeCharSequence.weighted(inputParts))

  private lazy val opt = OptionalParser(this)
  private lazy val rep0 = RepetitionParser(this, None, None)
  private lazy val rep1 = RepetitionParser(this, Some(1), None)

  /** Apply a transformation to the result of the left-hand parser. */
  def ^^  [B](transform: ParsingResult[A] => B): Parser[B] = TransformParser(this, transform)

  /** Apply a transformation to the product of the left-hand parser. */
  def ^*  [B](transform: A => B): Parser[B] = this ^^ { r => transform(r.product) }

  def ^^^ [B](transform: => B): Parser[B] = this ^^ { r => transform }

  def ? : OptionalParser[A] = opt

  def | [R <: Parser[_], C <: Parser[_]]
        (rhs: ParserMagnet[R])
        (implicit heuristic: OrderedChoiceHeuristic[this.type, R, C]): C =
    heuristic(this, rhs)

  def ||| [B >: A](rhs: Parser[B]): LongestChoiceParser[B] = LongestChoiceParser(this, rhs)

  def ~ [R <: Parser[_], S <: Parser[_]]
        (rhs: ParserMagnet[R])
        (implicit heuristic: SequencingHeuristic[this.type, R, S]): S =
    heuristic(this, rhs)

  /** Parse two expressions in sequence, discarding the results of the left-hand expression. */
  def ~> [B](rhs: Parser[B]): Parser[B] = RightParser(this, rhs)

  /** Parse two expressions in sequence, discarding the results of the right-hand expression. */
  def <~ [B](rhs: Parser[B]): Parser[A] = LeftParser(this, rhs)

  def *                       = rep0
  def *   (occurs: Int)       = RepetitionParser(this, Some(occurs),   Some(occurs)  )
  def *   (min: Int, max:Int) = RepetitionParser(this, Some(min),      Some(max)     )
  def *>  (min: Int)          = RepetitionParser(this, Some(min + 1),  None          )
  def *>= (min: Int)          = RepetitionParser(this, Some(min),      None          )
  def *<  (max: Int)          = RepetitionParser(this, None,           Some(max - 1) )
  def *<= (max: Int)          = RepetitionParser(this, None,           Some(max)     )
  def +                       = rep1

  def ^? [B](f: PartialFunction[ParsingResult[A], B]): Parser[B] = new Parser[B] {
    def parse(context: ParsingContext): ParsingResult[B] = {
      val r = lhs.parse(context)
      if(r.accepted && f.isDefinedAt(r)) r.copy(f(r)) else Rejected
    }
  }

  def ^*? [B](f: PartialFunction[A, B]): Parser[B] = new Parser[B] {
    def parse(context: ParsingContext): ParsingResult[B] = {
      val r = lhs.parse(context)
      if(r.accepted && f.isDefinedAt(r.product)) r.copy(f(r.product)) else Rejected
    }
  }
}

object Parser {
  /** [[smd.parsing.Parser]] matching the empty string, which always accepts and consumes no input. */
  object EmptyString extends Parser[Unit] {
    def parse(context: ParsingContext): ParsingResult[Unit] = context.resultBuilder.accept(Unit)
  }

  /** [[smd.parsing.Parser]] for the empty language, which never accepts and consumes no input. */
  object EmptyLanguage extends Parser[Nothing] {
    def parse(context: ParsingContext): ParsingResult[Nothing] = context.resultBuilder.reject[Nothing]
  }
}
