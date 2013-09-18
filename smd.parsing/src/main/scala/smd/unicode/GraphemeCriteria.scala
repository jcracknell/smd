package smd
package unicode

trait GraphemeCriterion { lhs =>
  /** Returns true iff the criterion is satisfied by the provided grapheme.
    *
    * @param grapheme the grapheme for which the satisfaction of the criterion should be verified.
    * @return true iff the criterion is satisfied by the provided grapheme.
    */
  def isSatisfiedBy(grapheme: GraphemeInfo): Boolean

  def &&(rhs: GraphemeCriterion): GraphemeCriterion = new GraphemeCriterion {
    def isSatisfiedBy(grapheme: GraphemeInfo): Boolean =
      lhs.isSatisfiedBy(grapheme) && rhs.isSatisfiedBy(grapheme)
  }

  def ||(rhs: GraphemeCriterion): GraphemeCriterion = new GraphemeCriterion {
    def isSatisfiedBy(grapheme: GraphemeInfo): Boolean =
      lhs.isSatisfiedBy(grapheme) || rhs.isSatisfiedBy(grapheme)
  }

  def unary_! : GraphemeCriterion = new GraphemeCriterion {
    def isSatisfiedBy(grapheme: GraphemeInfo): Boolean =
      !lhs.isSatisfiedBy(grapheme)
  }
}

/** Facility for the construction of [[smd.unicode.GraphemeCriterion]] instances. */
object GraphemeCriteria {
  /** [[smd.unicode.GraphemeCriterion]] which is always satisfied. */
  case object Any extends GraphemeCriterion {
    def isSatisfiedBy(grapheme: GraphemeInfo): Boolean = true
  }

  /** [[smd.unicode.GraphemeCriterion]] which is satisfied by any grapheme in the provided set of categories. */
  case class Category(categories: Set[UnicodeCategory]) extends GraphemeCriterion {
    private val acceptanceMap = Array.fill(UnicodeCategory.MaxValue + 1)(false)
    categories.foreach { cat => acceptanceMap(cat.value) = true }

    def isSatisfiedBy(grapheme: GraphemeInfo): Boolean = acceptanceMap(grapheme.category)
  }

  object Category {
    def apply(c0: UnicodeCategory, cs: UnicodeCategory*): Category = apply(c0 +: cs)
    def apply(categories: Iterable[UnicodeCategory]): Category = apply(categories.toSet)
  }

  /** [[smd.unicode.GraphemeCriterion]] which is never satisfied. */
  case object None extends GraphemeCriterion {
    def isSatisfiedBy(grapheme: GraphemeInfo): Boolean = false
  }

  /** [[smd.unicode.GraphemeCriterion]] which is satisfied by graphemes consisting of a single code point
    * satisfying the provided [[smd.unicode.CodePointCriterion]].
    *
    * @param criterion the [[smd.unicode.CodePointCriterion]] which must be satisfied.
    */
  case class SingleCodePoint(criterion: CodePointCriterion) extends GraphemeCriterion {
    def isSatisfiedBy(grapheme: GraphemeInfo): Boolean =
      grapheme.codePoints.lengthEq(1) && criterion.isSatisfiedBy(grapheme.codePoints.head)
  }
}
