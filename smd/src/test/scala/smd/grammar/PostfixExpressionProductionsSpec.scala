package smd
package grammar

import smd.dom.Expression

class PostfixExpressionProductionsSpec extends ProductionSpec {
  def subject = Grammar.PostfixExpression

  shouldParse("@a++") as Expression.PostfixIncrement(Expression.Identifier("a"))
  shouldParse("@a--") as Expression.PostfixDecrement(Expression.Identifier("a"))
}
