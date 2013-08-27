package smd
package parsing

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
