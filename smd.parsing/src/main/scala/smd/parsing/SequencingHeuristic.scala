package smd
package parsing

/** Defines a heuristic for combining two parsers into a resultant parser which applies the two argument parsers
  * in sequence.
  *
  * @tparam L the type of the left-hand parser.
  * @tparam R the type of the right-hand parser.
  * @tparam S the type of the resultant sequence parser.
  */
trait SequencingHeuristic[-L <: Parser[_], -R <: Parser[_], +S <: Parser[_]] extends ((L, R) => S) {
  /** Combine the provided left and right-hand parsers into a single parser which parses them in sequence.
    *
    * @param lhs the left-hand parser.
    * @param rhs the right-hand parser.
    */
  def apply(lhs: L, rhs: R): S
}

object SequencingHeuristic {
  /** This function uses type inferral to eliminate a lot of the boilerplate which would otherwise be required for the
    * creation of a [[smd.parsing.SequencingHeuristic]].
    *
    * @param comb function implementing the concatenation heuristic.
    * @tparam L the left-hand parser type (inferred).
    * @tparam R the right-hand parser type (inferred).
    * @tparam S the combined parser type (inferred).
    * @return a [[smd.parsing.SequencingHeuristic]] implementation which delegates to the provided `comb` function.
    */
  def create[L <: Parser[_], R <: Parser[_], S <: Parser[_]](comb: (L, R) => S): SequencingHeuristic[L, R, S] =
    new SequencingHeuristic[L, R, S] {
      def apply(lhs: L, rhs: R): S = comb(lhs, rhs)
    }
}
