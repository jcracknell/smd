package smd
package grammar

import smd.expression._
import smd.parsing.ParsingScenarios

class UnaryExpressionProductionsSpec extends ParsingScenarios {
  import Grammar.unaryExpression

  parsing("!true") as unaryExpression should produce (LogicalNot(BooleanLiteral(true)))
  parsing("--@a") as unaryExpression should produce (PrefixDecrement(Identifier("a")))
  parsing("-3") as unaryExpression should produce (Negative(NumericLiteral(3d)))
  parsing("++@foo") as unaryExpression should produce (PrefixIncrement(Identifier("foo")))
  parsing("+42e0") as unaryExpression should produce (Positive(NumericLiteral(42d)))
  parsing("~42") as unaryExpression should produce (BitwiseNot(NumericLiteral(42d)))
  parsing("typeof 42") as unaryExpression should produce (TypeOf(NumericLiteral(42d)))
  parsing("delete @a") as unaryExpression should produce (Delete(Identifier("a")))
  parsing("void null") as unaryExpression should produce (Void(NullLiteral()))
}
