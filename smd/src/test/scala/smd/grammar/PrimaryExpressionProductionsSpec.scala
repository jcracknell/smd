package smd
package grammar


class PrimaryExpressionProductionsSpec extends ProductionSpec {
  def subject = Grammar.PrimaryExpression

  shouldParse("[]")              as expression.ArrayLiteral(Seq())
  shouldParse("['a']")           as expression.ArrayLiteral(Seq(expression.QuotedStringLiteral("a")))
  shouldParse("['a',42]")        as expression.ArrayLiteral(Seq(expression.QuotedStringLiteral("a"), expression.NumericLiteral(42d)))
  shouldParse("[,]")             as expression.ArrayLiteral(Seq())
  shouldParse("[,'a']")          as expression.ArrayLiteral(Seq(expression.Elided(), expression.QuotedStringLiteral("a")))
  shouldParse("[,,'a','b',,,,]") as expression.ArrayLiteral(Seq(
                                   expression.Elided(),
                                   expression.Elided(),
                                   expression.QuotedStringLiteral("a"),
                                   expression.QuotedStringLiteral("b")
                                 ))
  shouldParse("{}")              as expression.ObjectLiteral(Seq())
  shouldParse("{'a':42}")        as expression.ObjectLiteral(Seq("a" -> expression.NumericLiteral(42d)))
  shouldParse("(@a || true)")    as expression.LogicalOr(expression.Identifier("a"), expression.BooleanLiteral(true))
}
