package smd
package parsing

import scala.util.Success

/** The result of applying an [[smd.parsing.Parser]] to an [[smd.parsing.ParsingContext]].
  *
  * @tparam A the type of the product of the parsing result.
  * @define ifAccepted (if parsing was successful)
  */
sealed abstract class ParsingResult[+A] {
  /** Returns true if parser accepted the input. */
  def accepted: Boolean

  /** Returns true if the parser rejected the input. */
  def rejected: Boolean

  /** Alias for `this.product`. */
  @throws[UnsupportedOperationException]
  @inline def p : A = product

  /** The product of the parser $ifAccepted. */
  @throws[UnsupportedOperationException]
  def product: A

  /** The index at which parsing started $ifAccepted. */
  @throws[UnsupportedOperationException]
  def index: Int

  /** The index prior to which parsing ended $ifAccepted. */
  @throws[UnsupportedOperationException]
  def endIndex: Int

  /** The number of consumed characters $ifAccepted. */
  @throws[UnsupportedOperationException]
  def length: Int

  /** The parsed sub-sequence of the input $ifAccepted. */
  @throws[UnsupportedOperationException]
  def parsed: CharSequence

  /** The range of input indices which was consumed. */
  @throws[UnsupportedOperationException]
  def range: Range = index until endIndex

  /** Creates a copy of this [[smd.parsing.ParsingResult]] with the provided replacement product.
    *
    * @param replacement the product to be stored in the resulting copy.
    * @tparam B the type of the product of the resulting copy.
    */
  @throws[UnsupportedOperationException]
  def copy[B](replacement: B): ParsingResult[B]
}

object ParsingResult {
  /** Creates a rejected [[smd.parsing.ParsingResult]] of the specified type.
    *
    * @tparam A the type of the failed [[smd.parsing.ParsingResult]] to be created.
    */
  def rejected[A]: ParsingResult[A] = Rejected.asInstanceOf[ParsingResult[A]]
}

/** An accepted [[smd.parsing.ParsingResult]]. */
class Accepted[+A](val product: A, protected val source: CharSequence, val index: Int, val endIndex: Int) extends ParsingResult[A] {
  def accepted: Boolean = true
  def rejected: Boolean = false
  def length: Int = endIndex - index
  def parsed: CharSequence = source.subSequenceProxy(index, endIndex)
  def copy[B](replacement: B): ParsingResult[B] = new Accepted[B](replacement, source, index, endIndex)

  override def hashCode(): Int = scala.runtime.ScalaRunTime._hashCode((product, source, index, endIndex))

  override def equals(obj: scala.Any): Boolean = obj match {
    case that: Accepted[_] => this.index   == that.index &&
                              this.length  == that.length &&
                              this.product == that.product &&
                              this.source  == that.source
    case _ => false
  }

  override def toString: String = s"${getClass.getName}($product)"
}

object Accepted {
  def unapply[A](parsingResult: ParsingResult[A]): Option[(A, Int, Int)] = parsingResult match {
    case p: Accepted[A] => Some((p.product, p.index, p.length))
    case _ => None
  }

  object Consuming {
    def unapply(parsingResult: ParsingResult[_]): Option[CharSequence] = parsingResult match {
      case p: Accepted[_] => Some(p.parsed)
      case _ => None
    }
  }

  object Producing {
    def unapply[A](parsingResult: ParsingResult[A]): Option[A] = parsingResult match {
      case a: Accepted[A] => Some(a.product)
      case _ => None
    }
  }
}

/** The singleton rejected [[smd.parsing.ParsingResult]]. */
case object Rejected extends ParsingResult[Nothing] {
  val accepted: Boolean = false
  val rejected: Boolean = true

  @inline private def unsupported[A](op: String): A =
    throw new UnsupportedOperationException(s"Attempt to access $op of ${getClass.getCanonicalName}.")

  def product: Nothing = unsupported("product")
  def index: Int = unsupported("index")
  def endIndex: Int = unsupported("index")
  def length: Int = unsupported("length")
  def parsed: CharSequence = unsupported("parsed")
  def copy[B](replacement: B): ParsingResult[B] = unsupported("copy")
}
