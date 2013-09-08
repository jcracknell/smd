package smd
package grammar


class PostfixExpressionProductionsSpec extends ProductionSpec {
  def subject = Grammar.PostfixExpression

  shouldParse("@a++") as expression.PostfixIncrement(expression.Identifier("a"))
  shouldParse("@a--") as expression.PostfixDecrement(expression.Identifier("a"))
}
