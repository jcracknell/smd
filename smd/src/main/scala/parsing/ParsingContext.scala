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
  def resultBuilder: ResultBuilder = new ResultBuilder

  class ResultBuilder {
    protected val resultIndex = context.index

    /** Creates a successful [[smd.parsing.ParsingResult]] containing the provided product.
      *
      * @param product the product contained by the resulting [[smd.parsing.ParsingResult]].
      * @tparam A the type of the product.
      * @return a successful [[smd.parsing.ParsingResult]] containing the provided product.
      */
    def success[A](product: A): ParsingResult[A] =
      new ParsingResult.Success[A](product, input, resultIndex, context.index)

    /** Creates an unsuccessful [[smd.parsing.ParsingResult]]. */
    def failure[A]: ParsingResult[A] = ParsingResult.Failure
  }
}
