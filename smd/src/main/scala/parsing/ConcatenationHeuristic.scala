package smd
package parsing

trait ConcatenationHeuristic[-L <: Parser[_], -R <: Parser[_], +C <: Parser[_]] {
  def concat(lhs: L, rhs: R): C
}

object ConcatenationHeuristic {
  /** This function uses type inferral to eliminate a lot of the boilerplate which would otherwise be required for the
    * creation of a [[smd.parsing.ConcatenationHeuristic]].
    *
    * @param comb function implementing the concatenation heuristic.
    * @tparam L the left-hand parser type (inferred).
    * @tparam R the right-hand parser type (inferred).
    * @tparam C the combined parser type (inferred).
    * @return a [[smd.parsing.ConcatenationHeuristic]] implementation which delegates to the provided `comb` function.
    */
  def create[L <: Parser[_], R <: Parser[_], C <: Parser[_]](comb: (L, R) => C): ConcatenationHeuristic[L, R, C] =
    new ConcatenationHeuristic[L, R, C] {
      def concat(lhs: L, rhs: R): C = comb(lhs, rhs)
    }
}
