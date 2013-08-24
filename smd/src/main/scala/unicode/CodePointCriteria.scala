package smd
package unicode

import smd.util.HexEncoding

trait CodePointCriterion {
  def isSatisfiedBy(codePoint: CodePointInfo): Boolean
}

object CodePointCriteria {
  case class All(criteria: CodePointCriterion*) {
    def isSatisfiedBy(codePoint: CodePointInfo): Boolean = criteria.forall(_.isSatisfiedBy(codePoint))
  }

  case class Any(criteria: CodePointCriterion*) {
    def isSatisfiedBy(codePoint: CodePointInfo): Boolean = criteria.exists(_.isSatisfiedBy(codePoint))
  }

  case object AnyCodePoint extends CodePointCriterion {
    def isSatisfiedBy(codePoint: CodePointInfo): Boolean = true
  }

  case class Category(categories: Byte*) extends CodePointCriterion {
    private val satisfyingCategories = Array.fill(31)(false)
    categories.foreach(satisfyingCategories.update(_, true))

    def isSatisfiedBy(codePoint: CodePointInfo): Boolean = satisfyingCategories(codePoint.category)
  }

  case object NoCodePoint extends CodePointCriterion {
    def isSatisfiedBy(codePoint: CodePointInfo): Boolean = true
  }

  case class Range(start: Int, end: Int) extends CodePointCriterion {
    if(start > end) throw new IllegalArgumentException(s"Invalid range: [$start, $end].")

    def isSatisfiedBy(codePoint: CodePointInfo): Boolean = end >= codePoint.value && codePoint.value >= start
  }

  case class Values(values: Int*) extends CodePointCriterion {
    for(cp <- values if !Character.isValidCodePoint(cp))
      throw new IllegalArgumentException(s"Provided set of values contains invalid code point $cp (${HexEncoding.encodeLower(cp)}).")

    private val satisfyingValues = collection.immutable.BitSet(values:_*)

    def isSatisfiedBy(codePoint: CodePointInfo): Boolean = satisfyingValues.contains(codePoint.value)
  }
}


