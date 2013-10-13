package smd
package grammar

import smd.expression._

class PostfixExpressionProductionsSpec extends ProductionSpec {
  import Grammar.postfixExpression

  parsing("@a++") as postfixExpression should produce (PostfixIncrement(Identifier("a")))
  parsing("@a--") as postfixExpression should produce (PostfixDecrement(Identifier("a")))
}
