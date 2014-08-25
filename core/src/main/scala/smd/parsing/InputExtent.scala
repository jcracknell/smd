package smd
package parsing

abstract class InputExtent extends CharSequence {
  def source: CharSequence
  def sourceIndexAt(i: Int): Int
  def sourceStartIndex: Int = sourceIndexAt(0)
  def sourceEndIndex: Int = sourceIndexAt(length)
  def subSequence(start: Int, end: Int): InputExtent

  override def charAt(i: Int): Char = source.charAt(sourceIndexAt(i))

  override def equals(obj: Any): Boolean = obj match {
    case that: InputExtent if this eq that => true
    case that: CharSequence =>
      this.length == that.length &&
      (0 until length forall (i => this.charAt(i) == that.charAt(i)))
    case _ => false
  }

  override def toString: String = {
    val sb = new StringBuilder(length)
    var i = 0
    while(i < length) { sb.append(charAt(i)); i += 1 }
    sb.toString
  }
}

object InputExtent {
  import scala.language.implicitConversions

  implicit def apply(source: CharSequence): InputExtent = source match {
    case ie: InputExtent => ie
    case _ => new Root(source)
  }

  def apply(parts: Seq[InputExtent]): InputExtent = Composite.balanced(parts)

  def unapply(extent: InputExtent): Option[CharSequence] = Some(extent.toString)


  case class Root(source: CharSequence) extends InputExtent {
    def sourceIndexAt(i: Int): Int = i
    def subSequence(start: Int, end: Int): InputExtent = new SubSequence(this, start, end)
    def length(): Int = source.length()

    override def hashCode(): Int = source.hashCode()
  }

  object Empty extends Root("")

  case class SubSequence(parent: InputExtent, startIndex: Int, endIndex: Int) extends InputExtent {
    private val len = endIndex - startIndex

    val source: CharSequence = parent.source
    def sourceIndexAt(i: Int): Int = parent.sourceIndexAt(startIndex + i)
    def subSequence(start: Int, end: Int): InputExtent = new SubSequence(parent, startIndex + start, startIndex + end)
    def length(): Int = len

    override def hashCode(): Int = parent.hashCode() ^ startIndex ^ endIndex
  }

  case class Composite(left: InputExtent, right: InputExtent) extends InputExtent {
    assert(left.source == right.source, "left and right sides must share a common source")

    private val split = left.length
    private val len = left.length + right.length

    val source: CharSequence = left.source
    def sourceIndexAt(i: Int): Int = if (i < split) left.sourceIndexAt(i) else right.sourceIndexAt(i - split)
    def length(): Int = len

    def subSequence(start: Int, end: Int): InputExtent =
      if (end <= split) left.subSequence(start, end)
      else if (split < start) right.subSequence(start - split, end - split)
      else new SubSequence(this, start, end)

    override def hashCode(): Int = left.hashCode() ^ right.hashCode()
  }

  object Composite {
    def balanced(parts: Seq[InputExtent]): InputExtent = {
      // Repeatedly combine elements of the parts list two at a time until there is only one part left
      def mkBalanced(parts: Seq[InputExtent]): List[InputExtent] = parts match {
        case Seq()                => Empty :: Nil
        case Seq(a)               => a :: Nil
        case Seq(a, b)            => new Composite(a, b) :: Nil
        case Seq(a, b, rest @ _*) => mkBalanced(new Composite(a, b) :: mkBalanced(rest))
      }
      mkBalanced(parts).head
    }
  }
}

