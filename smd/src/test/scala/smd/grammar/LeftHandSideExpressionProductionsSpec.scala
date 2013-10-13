package smd
package grammar

import smd.expression._

class LeftHandSideExpressionProductionsSpec extends ProductionSpec {
  import Grammar.leftHandSideExpression

  parsing("@a()") as leftHandSideExpression should produce (
    Call(
      body = Identifier("a"),
      args = Seq()
    )
  )

  parsing("@a.b") as leftHandSideExpression should produce (
    StaticProperty(
      body = Identifier("a"),
      member = "b"
    )
  )

  parsing("@a['b']") as leftHandSideExpression should produce (
    DynamicProperty(
      body = Identifier("a"),
      member = StringLiteral("b")
    )
  )

  parsing("@a.b()") as leftHandSideExpression should produce (
    Call(
      body = StaticProperty(
               body = Identifier("a"),
               member = "b"
             ),
      args = Seq()
    )
  )

  parsing("@a.b('c')") as leftHandSideExpression should produce (
    Call(
      body = StaticProperty(
        body = Identifier("a"),
        member = "b"
      ),
      args = Seq(StringLiteral("c"))
    )
  )
}
