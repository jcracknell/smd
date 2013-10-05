package smd
package parsing

/** Describes how to combine a pair of [[smd.parsing.Parser]] types into an [[smd.parsing.OrderedChoiceParser]].
  *
  * For reasons unknown, the upper bound on `C` must be `Parser[_]`, otherwise the upper bound of `L` and `R` does not
  * propagate.
  *
  * @tparam L the type of the left-hand [[smd.parsing.Parser]].
  * @tparam R the type of the right-hand [[smd.parsing.Parser]].
  * @tparam C the type of the resulting [[smd.parsing.OrderedChoiceParser]].
  */
abstract class OrderedChoiceHeuristic[-L <: Parser[_], -R <: Parser[_], +C <: Parser[_]] extends ((L, R) => C) {
  /** Apply this heuristic to the provided left and right-hand parsers.
    *
    * @param lhs the left-hand parser.
    * @param rhs the right-hand parser.
    */
  def apply(lhs: L, rhs: R): C
}

object OrderedChoiceHeuristic extends OrderedChoiceHeuristics.GeneratingTier
                                 with OrderedChoiceHeuristics.AddingTier
                                 with OrderedChoiceHeuristics.ConcatenatingTier

/** Encapsulates [[smd.parsing.OrderedChoiceHeuristic]] implementation fixtures. */
object OrderedChoiceHeuristics {
  trait Tier

  /** This class exists in order to introduce a covariant type parameter for the '''sole''' purpose of breaking
    * contravariant type inheritance as a workaround to scala's strange specificity rules for contravariant types,
    * under which `OrderedChoiceHeuristic[Parser[A], Parser[A], OrderedChoiceParser[A]]` is more specific than
    * `OrderedChoiceHeuristic[OrderedChoiceParser[A], OrderedChoiceParser[A], OrderedChoiceParser[A]]`, as the former
    * is assignable to the latter.
    *
    * See [[https://groups.google.com/forum/#!topic/scala-language/ZE83TvSWpT4]] for some insight into the contravariant
    * type specificity problem.
    *
    * @tparam T the tier to which the heuristic belongs.
    */
  abstract class TieredHeuristic[+T <: Tier, -L <: Parser[_], -R <: Parser[_], +C <: Parser[_]]
    extends OrderedChoiceHeuristic[L, R, C]

  /** Create a [[smd.parsing.OrderedChoiceHeuristics.TieredHeuristic]] for the specifiec tier.
    * This method eliminates a significant amount of boilerplate.
    *
    * @tparam T the tier of the resulting heuristic.
    */
  def createTiered[T <: Tier]: TieredHeuristicBuilder[T] = new TieredHeuristicBuilder[T]

  class TieredHeuristicBuilder[T <: Tier] {
    def apply[L <: Parser[_], R <: Parser[_], C <: OrderedChoiceParser[_]](comb: (L, R) => C): TieredHeuristic[T, L, R, C] =
      new TieredHeuristic[T, L, R, C] { def apply(lhs: L, rhs: R): C = comb(lhs, rhs) }
  }

  trait GeneratingTier extends Tier {
    /** Implicit [[smd.parsing.OrderedChoiceHeuristic]] combining an [[smd.parsing.Parser]] on the left-hand side with
      * an [[smd.parsing.Parser]] on the right hand side to produce an [[smd.parsing.OrderedChoiceParser]].
      */
    implicit def generatingHeuristic[A]: TieredHeuristic[GeneratingTier,
      /**  left: */ Parser[A],
      /** right: */ Parser[A],
      /**  dest: */ OrderedChoiceParser[A]
    ] =
      createTiered[GeneratingTier]((l, r) => OrderedChoiceParser(l, r))
  }

  trait AddingTier extends GeneratingTier {
    /** Implicit [[smd.parsing.OrderedChoiceHeuristic]] combining an [[smd.parsing.OrderedChoiceParser]] on the
      * left-hand side with an [[smd.parsing.OrderedChoiceParser]] on the right hand side to produce an
      * [[smd.parsing.OrderedChoiceParser]].  */
    implicit def prependingHeuristic[A]: TieredHeuristic[AddingTier,
      /**  left: */ Parser[A],
      /** right: */ OrderedChoiceParser[A],
      /**  dest: */ OrderedChoiceParser[A]
    ] =
      createTiered[AddingTier]((l, r) => OrderedChoiceParser(l +: r.choices))

    /** Implicit [[smd.parsing.OrderedChoiceHeuristic]] combining an [[smd.parsing.OrderedChoiceParser]] on the
      * left-hand side with an [[smd.parsing.Parser]] on the right hand side to produce an
      * [[smd.parsing.OrderedChoiceParser]].  */
    implicit def appendingHeuristic[A]: TieredHeuristic[AddingTier,
      /**  left: */ OrderedChoiceParser[A],
      /** right: */ Parser[A],
      /**  dest: */ OrderedChoiceParser[A]
    ] =
      createTiered[AddingTier]((l, r) => OrderedChoiceParser(l.choices :+ r))
  }

  trait ConcatenatingTier extends AddingTier {
    /** Implicit [[smd.parsing.OrderedChoiceHeuristic]] combining an [[smd.parsing.OrderedChoiceParser]] on the
      * left-hand side with an [[smd.parsing.OrderedChoiceParser]] on the right hand side to produce an
      * [[smd.parsing.OrderedChoiceParser]].  */
    implicit def concatenatingHeuristic[A]: TieredHeuristic[ConcatenatingTier,
      /**  left: */ OrderedChoiceParser[A],
      /** right: */ OrderedChoiceParser[A],
      /**  dest: */ OrderedChoiceParser[A]
    ] =
      createTiered[ConcatenatingTier]((l, r) => OrderedChoiceParser(l.choices ++ r.choices))
  }
}
