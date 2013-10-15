package smd
package util

import scala.annotation.tailrec

/** Concatenates two [[java.lang.CharSequence]] instances to represent a single [[java.lang.CharSequence]]
  * in order to conserve memory.
  *
  * @param left the left-hand [[java.lang.CharSequence]].
  * @param right the right-hand [[java.lang.CharSequence]].
  */
class CompositeCharSequence(val left: CharSequence, val right: CharSequence) extends CharSequence {
  private val split = left.length

  // Calculate and store the length for performance
  private val _length = left.length + right.length

  def length(): Int = _length

  def charAt(i: Int): Char = if(i < split) left.charAt(i) else right.charAt(i - split)

  def subSequence(start: Int, end: Int): CharSequence =
    if(end < split) subSequenceSide(left, start, end)
    else if(split <= start) subSequenceSide(right, start - split, end - split)
    else new ProxyCharSequence(this, start, end)

  private def subSequenceSide(side: CharSequence, start: Int, end: Int): CharSequence =
    side match {
      case ccs: CompositeCharSequence => ccs.subSequence(start, end)
      case _ => new ProxyCharSequence(side, start, end)
    }

  override def toString: String = appendTo(new StringBuilder(_length)).toString

  /** Appends the contents of this [[smd.util.CompositeCharSequence]] to the provide [[StringBuilder]]. */
  def appendTo(sb: StringBuilder): StringBuilder =
    appendSide(right, appendSide(left, sb))

  /** Appends the provided [[java.lang.CharSequence]] to the provided [[StringBuilder]], checking to see
    * if the process can be optimized in the case of an [[smd.util.CompositeCharSequence]]. */
  private def appendSide(side: CharSequence, sb: StringBuilder): StringBuilder =
    side match {
      case ccs: CompositeCharSequence => ccs.appendTo(sb)
      case _ => sb.append(side)
    }

  override def hashCode(): Int = left.hashCode ^ right.hashCode

  override def equals(obj: Any): Boolean = obj match {
    case that: CharSequence =>
      this.length == that.length &&
      (0 until length).forall(i => this.charAt(i) == that.charAt(i))
    case _ => false
  }
}

/** Provides facilities for constructing and deconstructing [[smd.util.CompositeCharSequence]] instances.
  *
  * Methods for constructing [[java.lang.CharSequence]] instances with a specific internal structure are
  * provided:
  * 
  * ''balanced'', which creates a [[java.lang.CharSequence]] whose $balancedDesc. Balanced structure 
  * is acheived in runtime $balancedRuntime.
  *
  * ''weighted'', which creates a [[java.lang.CharSequence]] whose $weightedDesc. Weighted structure is acheived
  * in runtime $weightedRuntime.
  *
  * A weighted tree structure may result in improved performance for large tree sizes.
  * 
  * @define composite
  *   Concatenates the provided [[java.lang.CharSequence]] instances to form a single [[java.lang.CharSequence]]
  *
  * @define parts the [[java.lang.CharSequence]] instances to be concatenated
  *
  * @define balancedDesc
  *   internal representation is that of a balanced binary search tree on the provided [[java.lang.CharSequence]]
  *   instances 
  *
  * @define balancedRuntime
  *   O(n), where n is the number of [[java.lang.CharSequence]] instances to be concatenated
  *
  * @define weightedDesc
  *   internal representation is that of a weighted binary search tree on the provided [[java.lang.CharSequence]]
  *   instances, in which the weight of a branch is the number of characters stored by the branch
  *
  * @define weightedRuntime 
  *   O(n/t⋅(ln n) + n) < O(n⋅(ln n)), where ''n'' is the number of [[java.lang.CharSequence]] instances
  *   to be concatenated, and ''t'' is the specified threshold on tree size beneath which balanced construction is
  *   employed
  */
object CompositeCharSequence {
  /** Creates a new [[smd.util.CompositeCharSequence]] combining the two provided [[java.lang.CharSequence]] values.
    *
    * @param left the left-hand [[java.lang.CharSequence]]
    * @param right the right-hand [[java.lang.CharSequence]]
    */
  def apply(left: CharSequence, right: CharSequence): CompositeCharSequence =
    new CompositeCharSequence(left, right)

  /** Deconstructor for [[smd.util.CompositeCharSequence]] instances; mostly for testing purposes. */
  def unapply(cs: CharSequence): Option[(CharSequence, CharSequence)] = cs match {
    case ccs: CompositeCharSequence => Some((ccs.left, ccs.right))
    case _ => None
  }

  /** $composite whose $balancedDesc.
    *
    * A balanced tree is constructed in runtime $balancedRuntime.
    *
    * @param parts $parts.
    */
  def balanced(parts: Seq[CharSequence]): CharSequence = {
    assert(!parts.isEmpty, "must provide at least one character sequence")

    val indexed = parts.toIndexedSeq
    mkBalanced(indexed, 0, indexed.length - 1)
  }

  private def mkBalanced(parts: IndexedSeq[CharSequence], start: Int, end: Int): CharSequence =
    if(start >= end) parts(start)
    else {
      val mid = start + (end - start >>> 1)
      new CompositeCharSequence(mkBalanced(parts, start, mid), mkBalanced(parts, mid + 1, end))
    }

  /** $composite whose $weightedDesc.
    *
    * A threshold on tree size `balancedBelow` can be specified, beneath which balanced construction is
    * to be used.
    *
    * A weighted tree is constructed in runtime $weightedRuntime.
    *
    * @param parts $parts.
    * @param balancedBelow threshold on tree size beneath which balanced construction is to be used.
    *        A non-positive value causes weighted construction to be used throughout.
    */
  def weighted(parts: Seq[CharSequence], balancedBelow: Int = 32): CharSequence = {
    assert(!parts.isEmpty, "must provide at least one character sequence")

    val indexed = parts.toIndexedSeq
    if(indexed.length < balancedBelow)
      balanced(indexed)
    else
      mkWeighted(indexed, balancedBelow)
  }

  private def mkWeighted(parts: IndexedSeq[CharSequence], balancedBelow: Int): CharSequence = {
    val cumulativeWeight = parts.scanLeft(0) { (cw, cs) => cw + cs.length }
    @inline def weightPreceding(i: Int) = cumulativeWeight(i)
    @inline def weightFollowing(i: Int) = cumulativeWeight(i + 1)

    // Locates midpoint index `m` s.t. `m` is the greatest index `start <= m < end` having
    // `weightFollowing(m) < tw`.
    @tailrec def midpoint(tw: Int, start: Int, end: Int): Int =
      if(start >= end) start
      else {
        // `m` is chosen s.t. `start < m <= end` so we can discard `m` in the right direction
        val m = start + (end - start >>> 1) + 1
        val mw = weightFollowing(m)

        if(tw < mw) midpoint(tw, start, m - 1)
        else if(mw < tw) midpoint(tw, m, end)
        else m
      }

    def mk(start: Int, end: Int): CharSequence =
      if(end - start < balancedBelow)
        mkBalanced(parts, start, end)
      else if(start >= end)
        parts(start)
      else {
        val targetWeight = {
          val sw = weightPreceding(start)
          val ew = weightFollowing(end)
          sw + (ew - sw >>> 1)
        }
        val m = midpoint(targetWeight, start, end)
        new CompositeCharSequence(mk(start, m), mk(m + 1, end))
      }

    mk(0, parts.length - 1)
  }
}
