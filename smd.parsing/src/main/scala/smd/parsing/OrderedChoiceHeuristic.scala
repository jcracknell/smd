package smd
package parsing

trait OrderedChoiceHeuristic[-L <: Parser[_], -R <: Parser[_], +C <: Parser[_]] extends ((L, R) => C) {
  def apply(lhs: L, rhs: R): C
}

object OrderedChoiceHeuristic {
  def create[L <: Parser[_], R <: Parser[_], C <: Parser[_]](comb: (L, R) => C): OrderedChoiceHeuristic[L, R, C] =
    new OrderedChoiceHeuristic[L, R, C] {
      def apply(lhs: L, rhs: R): C = comb(lhs, rhs)
    }
}
