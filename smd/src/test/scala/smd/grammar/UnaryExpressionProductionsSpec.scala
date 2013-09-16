package smd
package grammar

class UnaryExpressionProductionsSpec extends ProductionSpec {
  def subject = Grammar.unaryExpression

  shouldParse("!true")     as expression.LogicalNot(expression.BooleanLiteral(true))
  shouldParse("--@a")      as expression.PrefixDecrement(expression.Identifier("a"))
  shouldParse("-3")        as expression.Negative(expression.NumericLiteral(3d))
  shouldParse("++@foo")    as expression.PrefixIncrement(expression.Identifier("foo"))
  shouldParse("+42e0")     as expression.Positive(expression.NumericLiteral(42d))
  shouldParse("~42")       as expression.BitwiseNot(expression.NumericLiteral(42d))
  shouldParse("typeof 42") as expression.TypeOf(expression.NumericLiteral(42d))
  shouldParse("delete @a") as expression.Delete(expression.Identifier("a"))
  shouldParse("void null") as expression.Void(expression.NullLiteral())
}
