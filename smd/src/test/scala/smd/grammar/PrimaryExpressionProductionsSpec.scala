package smd
package grammar


class PrimaryExpressionProductionsSpec extends ProductionSpec {
  def subject = Grammar.primaryExpression

  shouldParse("[]")              as expression.ArrayLiteral(Seq())
  shouldParse("['a']")           as expression.ArrayLiteral(Seq(expression.StringLiteral("a")))
  shouldParse("['a',42]")        as expression.ArrayLiteral(Seq(expression.StringLiteral("a"), expression.NumericLiteral(42d)))
  shouldParse("[,]")             as expression.ArrayLiteral(Seq())
  shouldParse("[,'a']")          as expression.ArrayLiteral(Seq(expression.Elided(), expression.StringLiteral("a")))
  shouldParse("[,,'a','b',,,,]") as expression.ArrayLiteral(Seq(
                                   expression.Elided(),
                                   expression.Elided(),
                                   expression.StringLiteral("a"),
                                   expression.StringLiteral("b")
                                 ))
  shouldParse("{}")              as expression.ObjectLiteral(Seq())
  shouldParse("{'a':42}")        as expression.ObjectLiteral(Seq("a" -> expression.NumericLiteral(42d)))
  shouldParse("(@a || true)")    as expression.LogicalOr(expression.Identifier("a"), expression.BooleanLiteral(true))
}
