package smd
package dom

sealed abstract class SourceRange {
  /** The start index of the source range, inclusive. */
  @throws[UnsupportedOperationException]
  def start: Int

  /** The end index of the source range, exclusive. */
  @throws[UnsupportedOperationException]
  def end: Int

  /** The length of the source range. */
  @throws[UnsupportedOperationException]
  def length: Int 

  override def equals(obj: scala.Any): Boolean = obj match {
    case SourceRange.Unknown => true
    case that: SourceRange => this.start == that.start && this.end == that.end
    case _ => false
  }
}

object SourceRange {
  def apply(start: Int, end: Int): SourceRange = new Value(start, end)

  def unapply(obj: SourceRange): Option[(Int, Int)] = obj match {
    case Value(s, e) => Some((s, e))
    case _ => None
  }

  case class Value(start: Int, end: Int) extends SourceRange {
    assert(start >= 0, "start must be a non-negative integer")
    assert(end >= 0, "end must be a non-negative integer")
    assert(end >= start, "must be a valid range")

    def length: Int = end - start
  }

  /** Special [[smd.dom.SourceRange]] value which is always equal to any other [[smd.dom.SourceRange]]. */
  case object Unknown extends SourceRange {
    @inline private def unsupported(op: String): Nothing =
      throw new UnsupportedOperationException(s"Attempt to access $op of ${getClass.getCanonicalName}.")

    def start: Int = unsupported("start")
    def end: Int = unsupported("end")
    def length: Int = unsupported("length")

    override def equals(obj: scala.Any): Boolean = obj match {
      case _: SourceRange => true
      case _ => false
    }

    override def toString: String = "Unknown"
  }
}

