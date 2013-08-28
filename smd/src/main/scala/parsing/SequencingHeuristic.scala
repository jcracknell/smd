package smd
package parsing

trait SequencingHeuristic[-L <: Parser[_], -R, +C <: Parser[_]] {
  def concat(lhs: L, rhs: R): C
}

object SequencingHeuristic {
  /** This function uses type inferral to eliminate a lot of the boilerplate which would otherwise be required for the
    * creation of a [[smd.parsing.SequencingHeuristic]].
    *
    * @param comb function implementing the concatenation heuristic.
    * @tparam L the left-hand parser type (inferred).
    * @tparam R the right-hand parser type (inferred).
    * @tparam C the combined parser type (inferred).
    * @return a [[smd.parsing.SequencingHeuristic]] implementation which delegates to the provided `comb` function.
    */
  def create[L <: Parser[_], R, C <: Parser[_]](comb: (L, R) => C): SequencingHeuristic[L, R, C] =
    new SequencingHeuristic[L, R, C] {
      def concat(lhs: L, rhs: R): C = comb(lhs, rhs)
    }
}
