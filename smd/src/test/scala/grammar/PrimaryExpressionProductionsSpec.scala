package smd
package grammar

import smd.dom.Expression

class PrimaryExpressionProductionsSpec extends ProductionSpec {
  def subject = Grammar.PrimaryExpression

  shouldParse("[]")              as Expression.ArrayLiteral(Seq())
  shouldParse("['a']")           as Expression.ArrayLiteral(Seq(Expression.StringLiteral("a")))
  shouldParse("['a',42]")        as Expression.ArrayLiteral(Seq(Expression.StringLiteral("a"), Expression.NumericLiteral(42d)))
  shouldParse("[,]")             as Expression.ArrayLiteral(Seq())
  shouldParse("[,'a']")          as Expression.ArrayLiteral(Seq(Expression.Elided(), Expression.StringLiteral("a")))
  shouldParse("[,,'a','b',,,,]") as Expression.ArrayLiteral(Seq(
                                   Expression.Elided(),
                                   Expression.Elided(),
                                   Expression.StringLiteral("a"),
                                   Expression.StringLiteral("b")
                                 ))
  shouldParse("{}")              as Expression.ObjectLiteral(Seq())
  shouldParse("{'a':42}")        as Expression.ObjectLiteral(Seq("a" -> Expression.NumericLiteral(42d)))
  shouldParse("(@a || true)")    as Expression.LogicalOr(Expression.Identifier("a"), Expression.BooleanLiteral(true))
}
