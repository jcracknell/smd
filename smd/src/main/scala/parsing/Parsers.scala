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

trait Parser[+A] extends (ParsingContext => ParsingResult[A]) {
  import Parser._

  def parse(context: ParsingContext): ParsingResult[A]
  override def apply(context: ParsingContext): ParsingResult[A] = parse(context)

  def ^^[B](transform: ParsingResult[A] => B): Parser[B] = Transform(this, transform)
}

object Parser {
  case class Literal(literal: String) extends Parser[String] {
    import smd.unicode.GraphemeInfo

    if(0 == literal.length) throw new IllegalArgumentException(s"Provided literal ${literal.literalEncode} has length 0.")

    private val lastGrapheme = GraphemeInfo.iterable(literal).last

    def parse(context: ParsingContext): ParsingResult[String] = {
      var i = 0
      while(i != literal.length) {
        if(literal.charAt(i) != context.input.charAt(context.index + i))
          return Failure
        i += 1
      }

      // Having checked sequence equivalence, we must also verify that the final grapheme in the literal fully matches
      // the grapheme at the corresponding location in the input in order to handle the case where the matching section
      // in the input is followed by additional combining marks.
      val finalContextGrapheme = GraphemeInfo.at(context.input, context.index + lastGrapheme.index)
      if(lastGrapheme.length != finalContextGrapheme.length)
        return Failure

      val result = Success(literal, context.index, literal.length)
      context.advanceBy(literal.length)
      result
    }
  }

  case class Transform[A, +B](parser: Parser[A], transform: ParsingResult[A] => B) extends Parser[B] {
    def parse(context: ParsingContext): ParsingResult[B] = {
      val r = parser.parse(context)
      if(r.succeeded) Success(transform(r), r.index, r.length) else Failure
    }
  }
}
