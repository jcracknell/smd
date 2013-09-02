package smd
package parsing

/** The result of applying an [[smd.parsing.Parser]] to an [[smd.parsing.ParsingContext]].
  *
  * @tparam A the type of the product of the parsing result.
  * @define ifSuccessful (if parsing was successful)
  */
sealed trait ParsingResult[+A] {
  /** Returns true if parsing was successful. */
  def succeeded: Boolean

  /** Returns true if parsing failed. */
  def failed: Boolean

  /** Alias for `this.product`. */
  @throws[UnsupportedOperationException]
  @inline def p : A = product

  /** The product of the parser $ifSuccessful. */
  @throws[UnsupportedOperationException]
  def product: A

  /** The index at which parsing started $ifSuccessful. */
  @throws[UnsupportedOperationException]
  def index: Int

  /** The index prior to which parsing ended $ifSuccessful. */
  @throws[UnsupportedOperationException]
  def endIndex: Int

  /** The number of consumed characters $ifSuccessful. */
  @throws[UnsupportedOperationException]
  def length: Int

  /** The parsed sub-sequence of the input $ifSuccessful. */
  @throws[UnsupportedOperationException]
  def parsed: CharSequence

  /** The range of input indices which was consumed. */
  @throws[UnsupportedOperationException]
  def range: Range = index until endIndex

  /** Creates a copy of this [[smd.parsing.ParsingResult]] with the provided replacement product.
    *
    * @param replacement the product to be stored in the resulting copy.
    * @tparam B the type of the product of the resulting copy.
    * @return a copy of this [[smd.parsing.ParsingResult]] with the provided replacement product.
    */
  @throws[UnsupportedOperationException]
  def copy[B](replacement: B): ParsingResult[B]
}

object ParsingResult {
  class Success[+A](val product: A, protected val source: CharSequence, val index: Int, val endIndex: Int) extends ParsingResult[A] {
    def succeeded: Boolean = true
    def failed: Boolean = false
    def length: Int = endIndex - index
    def parsed: CharSequence = source.proxySubSequence(index, endIndex)
    def copy[B](replacement: B): ParsingResult[B] = new Success[B](replacement, source, index, endIndex)
  }

  /** Creates a failed [[smd.parsing.ParsingResult]] of the specified type.
    *
    * @tparam A the type of the failed [[smd.parsing.ParsingResult]] to be created.
    * @return a failed [[smd.parsing.ParsingResult]] of the specified type.
    */
  def failure[A]: ParsingResult[A] = Failure.asInstanceOf[ParsingResult[A]]

  case object Failure extends ParsingResult[Nothing] {
    val succeeded: Boolean = false
    val failed: Boolean = true
    def product: Nothing =
      throw new UnsupportedOperationException("Attempt to access product of failed ParsingResult.")
    def index: Int =
      throw new UnsupportedOperationException("Attempt to access index of failed ParsingResult.")
    def endIndex: Int =
      throw new UnsupportedOperationException("Attempt to access endIndex of failed ParsingResult.")
    def length: Int =
      throw new UnsupportedOperationException("Attempt to access length of failed ParsingResult.")
    def parsed: CharSequence =
      throw new UnsupportedOperationException("Attempt to access parsedInput of failed ParsingResult.")
    def copy[B](replacement: B): ParsingResult[B] =
      throw new UnsupportedOperationException("Call to copy on failed ParsingResult.")
  }
}
