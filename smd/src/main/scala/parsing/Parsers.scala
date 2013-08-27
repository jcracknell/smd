package smd
package parsing

class ParsingContext(val input: String, protected var _index: Int) { context =>
  def this(input: String) = this(input, 0)

  val inputLength = input.length

  def index: Int = _index

  @inline def advanceBy(length: Int): Unit = advanceTo(_index + length)

  def advanceTo(index: Int): Unit = {
    if(index >= inputLength) throw new IllegalArgumentException(s"Provided index $index exceeds input length of $inputLength.")

    _index += index
  }

  override def clone: ParsingContext = ???

  def assimilate(clone: ParsingContext): Unit = {
    _index = clone._index
  }

  /** Mark the start of a parsing result. */
  def mark = new {
    val index = context._index
    def length = context._index - index
    def when[A](cond: Boolean, product: => A): ParsingResult[A] = if(cond) Success(product, index, length) else Failure
    def success[A](product: A): ParsingResult[A] = Success(product, index, length)
    def failure[A]: ParsingResult[A] = Failure
  }
}

/** The result of applying an [[smd.parsing.Parser]] to an [[smd.parsing.ParsingContext]]. */
sealed trait ParsingResult[+A] {
  /** The product of the parser, if parsing was successful. */
  @throws[UnsupportedOperationException]
  def product: A

  /** The index at which parsing started if parsing was successful. */
  @throws[UnsupportedOperationException]
  def index: Int

  /** The number of consumed characters, if parsing was successful. */
  @throws[UnsupportedOperationException]
  def length: Int

  /** Returns true if parsing was successful. */
  def succeeded: Boolean

  /** Returns true if parsing failed. */
  def failed: Boolean
}

object ParsingResult {
  /** Returns [[smd.parsing.Success]] if `cond` holds, [[smd.parsing.Failure]] otherwise.
    *
    * @param cond the condition which must hold for success.
    * @param result the result in the event of success.
    * @tparam A the type of the product in the event of success.
    * @return [[smd.parsing.Success]] if `cond` holds, [[smd.parsing.Failure]] otherwise.@return
    */
  // TODO: this is a littly wonky because of the multiple arguments. Is this method really necessary, or
  // will Parsers generally use the context tooling?
  def when[A](cond: Boolean, result: => ParsingResult[A]): ParsingResult[A] = if(cond) result else Failure

  /** Creates a failed [[smd.parsing.ParsingResult]] of the specified type.
    *
    * @tparam A the type of the failed [[smd.parsing.ParsingResult]] to be created.
    * @return a failed [[smd.parsing.ParsingResult]] of the specified type.
    */
  def failure[A]: ParsingResult[A] = Failure.asInstanceOf[ParsingResult[A]]
}

case class Success[+A](product: A, index: Int, length: Int) extends ParsingResult[A] {
  val succeeded: Boolean = true
  val failed:    Boolean = false
}

case object Failure extends ParsingResult[Nothing] {
  def product:   Nothing = throw new UnsupportedOperationException("Attempt to access product of failed ParsingResult.")
  def index:     Int =     throw new UnsupportedOperationException("Attempt to access index of failed ParsingResult.")
  def length:    Int =     throw new UnsupportedOperationException("Attempt to access length of failed ParsingResult.")
  val succeeded: Boolean = false
  val failed:    Boolean = true
}

trait Parsers {
  implicit def string2LiteralParser(str: String): LiteralParser = LiteralParser(str)
}

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
