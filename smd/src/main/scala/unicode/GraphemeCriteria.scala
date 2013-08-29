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

  case class Category(categories: Byte*) extends GraphemeCriterion {
    if(categories.exists(c => c > 30 || 0 > c)) throw new IllegalArgumentException(s"Provided category list contains an invalid category.")

    private val acceptanceMap = Array.fill(31)(false)
    categories.foreach(acceptanceMap.update(_, true))

    def isSatisfiedBy(grapheme: GraphemeInfo): Boolean = acceptanceMap(grapheme.category)
  }

  object Category {
    def apply(categories: Iterable[Byte]): Category = Category(categories.toSeq:_*)
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
    def isSatisfiedBy(grapheme: GraphemeInfo): Boolean = {
      ???
    }
  }
}
