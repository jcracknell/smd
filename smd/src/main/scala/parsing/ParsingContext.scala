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
