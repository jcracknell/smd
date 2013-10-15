package smd
package grammar

import smd.expression._
import smd.parsing.ParsingScenarios

class PrimaryExpressionProductionsSpec extends ParsingScenarios {
  import Grammar.primaryExpression

  parsing("[]") as primaryExpression should produce (ArrayLiteral(Seq()))

  parsing("['a']") as primaryExpression should produce (ArrayLiteral(Seq(StringLiteral("a"))))

  parsing("['a',42]") as primaryExpression should produce (ArrayLiteral(Seq(StringLiteral("a"), NumericLiteral(42d))))

  parsing("[,]") as primaryExpression should produce (ArrayLiteral(Seq()))

  parsing("[,'a']") as primaryExpression should produce (ArrayLiteral(Seq(Elided(), StringLiteral("a"))))

  parsing("[,,'a','b',,,,]") as primaryExpression should produce (
    ArrayLiteral(Seq(
      Elided(),
      Elided(),
      StringLiteral("a"),
      StringLiteral("b")
    ))
  )

  parsing("{}") as primaryExpression should produce (ObjectLiteral())

  parsing("{'a':42}") as primaryExpression should produce (ObjectLiteral("a" -> NumericLiteral(42d)))

  parsing("(@a || true)") as primaryExpression should produce ( LogicalOr(Identifier("a"), BooleanLiteral(true)))
}
