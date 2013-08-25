package smd
package unicode

trait GraphemeCriterion {
  /** Returns true iff the criterion is satisfied by the provided grapheme.
    *
    * @param grapheme the grapheme for which the satisfaction of the criterion should be verified.
    * @return true iff the criterion is satisfied by the provided grapheme.
    */
  def isSatisfiedBy(grapheme: GraphemeInfo): Boolean
}

/** Facility for the construction of [[smd.unicode.GraphemeCriterion]] instances. */
object GraphemeCriteria {
  /** [[smd.unicode.GraphemeCriterion]] which is satisfied if all of its constituent criteria are satisfied. */
  case class All(criteria: GraphemeCriterion*) extends GraphemeCriterion {
    def isSatisfiedBy(grapheme: GraphemeInfo): Boolean =
      criteria.forall(_.isSatisfiedBy(grapheme))
  }

  /** [[smd.unicode.GraphemeCriterion]] which is satisfied if any of its constituent criteria are satisfied. */
  case class Any(criteria: GraphemeCriterion*) extends GraphemeCriterion {
    def isSatisfiedBy(grapheme: GraphemeInfo): Boolean =
      criteria.exists(_.isSatisfiedBy(grapheme))
  }

  /** [[smd.unicode.GraphemeCriterion]] which is always satisfied. */
  case object AnyGrapheme extends GraphemeCriterion {
    def isSatisfiedBy(grapheme: GraphemeInfo): Boolean = true
  }

  case class Category(categories: Byte*) extends GraphemeCriterion {
    private val acceptanceMap = Array.fill(31)(false)
    categories.foreach(acceptanceMap.update(_, true))

    def isSatisfiedBy(grapheme: GraphemeInfo): Boolean = acceptanceMap(grapheme.category)
  }

  /** [[smd.unicode.GraphemeCriterion]] which is never satisfied. */
  case object NoGrapheme extends GraphemeCriterion {
    def isSatisfiedBy(grapheme: GraphemeInfo): Boolean = false
  }

  /** [[smd.unicode.GraphemeCriterion]] which is satisfied when the provided criterion is not satisfied. */
  case class Not(criterion: GraphemeCriterion) extends GraphemeCriterion {
    def isSatisfiedBy(grapheme: GraphemeInfo): Boolean = !criterion.isSatisfiedBy(grapheme)
  }

  /** [[smd.unicode.GraphemeCriterion]] which is satisfied by graphemes consisting of a single code point
    * satisfying the provided [[smd.unicode.CodePointCriterion]].
    *
    * @param criterion the [[smd.unicode.CodePointCriterion]] which must be satisfied.
    */
  case class SingleCodePoint(criterion: CodePointCriterion) extends GraphemeCriterion {
    def isSatisfiedBy(grapheme: GraphemeInfo): Boolean = {
      ???
    }
  }
}
