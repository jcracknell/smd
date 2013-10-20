package smd
package grammar

import smd.dom._
import smd.parsing.ParsingScenarios

class PostfixExpressionProductionsSpec extends ParsingScenarios {
  import Grammar.postfixExpression

  parsing("@a++") as postfixExpression should produce (PostfixIncrement(Identifier("a")))
  parsing("@a--") as postfixExpression should produce (PostfixDecrement(Identifier("a")))
}
