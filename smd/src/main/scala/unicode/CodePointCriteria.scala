package smd
package unicode

import smd.util.HexEncoding

trait CodePointCriterion { criterion =>
  def isSatisfiedBy(codePoint: CodePointInfo): Boolean

  def &&(rhs: CodePointCriterion): CodePointCriterion = new CodePointCriterion {
    def isSatisfiedBy(codePoint: CodePointInfo): Boolean =
      criterion.isSatisfiedBy(codePoint) && rhs.isSatisfiedBy(codePoint)
  }

  def ||(rhs: CodePointCriterion): CodePointCriterion = new CodePointCriterion {
    def isSatisfiedBy(codePoint: CodePointInfo): Boolean =
      criterion.isSatisfiedBy(codePoint) || rhs.isSatisfiedBy(codePoint)
  }

  def unary_! : CodePointCriterion = new CodePointCriterion {
    def isSatisfiedBy(codePoint: CodePointInfo): Boolean =
      !criterion.isSatisfiedBy(codePoint)
  }
}

object CodePointCriteria {
  /** An [[smd.unicode.CodePointCriterion]] which is always satisfied. */
  case object Any extends CodePointCriterion {
    def isSatisfiedBy(codePoint: CodePointInfo): Boolean = true
  }

  /** An [[smd.unicode.CodePointCriterion]] which is satisfied by any codepoint in the enumerated categories.
    *
    * @param categories categories which will satisfy this criterion.
    */
  case class Category(categories: Byte*) extends CodePointCriterion {
    private val satisfyingCategories = Array.fill(31)(false)
    categories.foreach(satisfyingCategories.update(_, true))

    def isSatisfiedBy(codePoint: CodePointInfo): Boolean = satisfyingCategories(codePoint.category)
  }

  /** An [[smd.unicode.CodePointCriterion]] which is never satisfied. */
  case object None extends CodePointCriterion {
    def isSatisfiedBy(codePoint: CodePointInfo): Boolean = false
  }

  /** An [[smd.unicode.CodePointCriterion]] which is satisfied by code points in the specified range of values.
    *
    * @param start the start of the range of accepted code points, inclusive.
    * @param end the end of the range of accepted code points, inclusive.
    */
  case class Range(start: Int, end: Int) extends CodePointCriterion {
    if(start > end) throw new IllegalArgumentException(s"Invalid range: [$start, $end].")

    def isSatisfiedBy(codePoint: CodePointInfo): Boolean = end >= codePoint.value && codePoint.value >= start
  }

  /** An [[smd.unicode.CodePointCriterion]] which is satisfied by any of the provided code point values.
    *
    * @param values the set of code points satisfying this criterion.
    */
  case class Values(values: Set[Int]) extends CodePointCriterion {
    for(cp <- values if !Character.isValidCodePoint(cp))
      throw new IllegalArgumentException(s"Provided set of values contains invalid code point $cp (${HexEncoding.encodeLower(cp)}).")

    def isSatisfiedBy(codePoint: CodePointInfo): Boolean = values.contains(codePoint.value)
  }

  object Values {
    private def mkValues(cps: Iterable[Int]) = Values(collection.immutable.BitSet(cps.toSeq: _*))

    def apply(cp0: Int, cps: Int*): Values = mkValues(cp0 +: cps)
    def apply[A <% Int](as: Iterable[A]): Values = mkValues(as.map(_.toInt))
  }
}


