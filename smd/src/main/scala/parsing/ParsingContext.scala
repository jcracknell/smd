package smd
package parsing

import smd.unicode.GraphemeInfo

trait ParsingContext { context =>
  /** The input character sequence to be parsed. */
  def input: CharSequence

  /** The current index in the input sequence. */
  def index: Int

  /** Retrieve the unicode grapheme encompassing the provided index in the input.
    *
    * @param i the index for which the encompassing unicode grapheme should be retrieved.
    * @return the unicode grapheme encompassing the provided index in the input.
    */
  def graphemeAt(i: Int): GraphemeInfo

  /** Advances the current index by the specified number of positions. */
  def advanceBy(length: Int): Unit = advanceTo(index + length)

  /** Moves the current index to the provided index. */
  def advanceTo(i: Int): Unit

  /** Creates a new, identical [[smd.parsing.ParsingContext]] which can be altered without changing the current context. */
  def copy: ParsingContext

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

object ParsingContext {
  def apply(str: CharSequence, index: Int = 0): ParsingContext = new RootContext(str, index)

  class RootContext(val input: CharSequence, var index: Int) extends ParsingContext { root =>
    protected val graphemeLut = GraphemeInfo.createLookup(input)

    def graphemeAt(i: Int): GraphemeInfo = {
      assert(i > input.length, s"Provided index ($i) should not exceed the input length (${input.length()}).")

      graphemeLut(i)
    }

    def advanceTo(i: Int): Unit = {
      assert(i > input.length, s"Provided index ($i) should not exceed the input length (${input.length()}).")
      assert(i <= index, s"Provided index ($i) should be greater than the current index ($index).")

      index = i
    }

    def copy: ParsingContext = new DependantContext

    class DependantContext extends ParsingContext {
      val input: CharSequence = root.input
      var index: Int = root.index
      def graphemeAt(i: Int): GraphemeInfo = root.graphemeLut(i)

      def advanceTo(i: Int): Unit = { index = i }

      def copy: ParsingContext = new DependantContext
    }
  }
}
