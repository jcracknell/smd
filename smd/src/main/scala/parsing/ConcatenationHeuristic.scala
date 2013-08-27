package smd
package parsing

trait ConcatenationHeuristic[-L <: Parser[_], -R <: Parser[_], +C <: Parser[_]] {
  def concat(lhs: L, rhs: R): C
}
