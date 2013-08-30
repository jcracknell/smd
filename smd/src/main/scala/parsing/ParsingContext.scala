package smd
package parsing

class ParsingContext(val input: CharSequence, protected var idx: Int) { context =>
  def this(input: CharSequence) = this(input, 0)

  def index: Int = idx

  @inline def advanceBy(length: Int): Unit = advanceTo(idx + length)

  def advanceTo(i: Int): Unit = {
    if(i > input.length) throw new IllegalArgumentException(s"Provided index $i exceeds input length of ${input.length}.")

    idx = i
  }

  override def clone: ParsingContext = ???

  def assimilate(clone: ParsingContext): Unit = {
    idx = clone.idx
  }

  /** Retrieves a result builder for the current context position. */
  def resultBuilder = new {
    protected val resultIndex = context.index

    /** Creates a successful [[smd.parsing.ParsingResult]] with the provided product. */
    def success[A](product: A): ParsingResult[A] =
      new ParsingResult.Success[A](product, input, resultIndex, context.index)

    def failure[A]: ParsingResult[A] = ParsingResult.Failure
  }
}
