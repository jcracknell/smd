package smd
package grammar

import smd.dom._
import smd.parsing.ParsingScenarios

class UnaryExpressionProductionsSpec extends ParsingScenarios {
  import Grammar.unaryExpression

  parsing("!true") as unaryExpression should produce (LogicalNot(BooleanLiteral(true)))
  parsing("-3") as unaryExpression should produce (Negative(NumericLiteral(3d)))
  parsing("+42e0") as unaryExpression should produce (Positive(NumericLiteral(42d)))
  parsing("~42") as unaryExpression should produce (BitwiseNot(NumericLiteral(42d)))
}
