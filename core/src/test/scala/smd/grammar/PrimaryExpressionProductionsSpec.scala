package smd
package grammar

import smd.dom._
import smd.parsing.ParsingScenarios

class PrimaryExpressionProductionsSpec extends ParsingScenarios {
  import Grammar.primaryExpression

  parsing("[]") as primaryExpression should produce (ArrayLiteral(Seq()))

  parsing("['a']") as primaryExpression should produce (ArrayLiteral(Seq(Some(StringLiteral("a")))))

  parsing("['a',42]") as primaryExpression should produce (ArrayLiteral(Seq(Some(StringLiteral("a")), Some(NumericLiteral(42d)))))

  parsing("[,]") as primaryExpression should produce (ArrayLiteral(Seq()))

  parsing("[,'a']") as primaryExpression should produce (ArrayLiteral(Seq(None, Some(StringLiteral("a")))))

  parsing("[,,'a','b',,,,]") as primaryExpression should produce (
    ArrayLiteral(Seq(
      None,
      None,
      Some(StringLiteral("a")),
      Some(StringLiteral("b"))
    ))
  )

  parsing("{}") as primaryExpression should produce (ObjectLiteral(Seq()))

  parsing("{'a'=42}") as primaryExpression should produce (ObjectLiteral(Seq("a" -> NumericLiteral(42d))))

  parsing("(@a || true)") as primaryExpression should produce ( LogicalOr(Identifier("a"), BooleanLiteral(true)))
}
