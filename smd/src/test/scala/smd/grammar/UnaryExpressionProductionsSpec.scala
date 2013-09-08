package smd
package grammar

import smd.dom.Expression

class UnaryExpressionProductionsSpec extends ProductionSpec {
  def subject = Grammar.UnaryExpression

  shouldParse("!true")     as Expression.LogicalNot(Expression.BooleanLiteral(true))
  shouldParse("--@a")      as Expression.PrefixDecrement(Expression.Identifier("a"))
  shouldParse("-3")        as Expression.Negative(Expression.NumericLiteral(3d))
  shouldParse("++@foo")    as Expression.PrefixIncrement(Expression.Identifier("foo"))
  shouldParse("+42e0")     as Expression.Positive(Expression.NumericLiteral(42d))
  shouldParse("~42")       as Expression.BitwiseNot(Expression.NumericLiteral(42d))
  shouldParse("typeof 42") as Expression.Typeof(Expression.NumericLiteral(42d))
  shouldParse("delete @a") as Expression.Delete(Expression.Identifier("a"))
  shouldParse("void null") as Expression.Void(Expression.NullLiteral())
}
