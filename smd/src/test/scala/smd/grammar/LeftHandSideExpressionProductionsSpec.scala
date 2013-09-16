package smd
package grammar

class LeftHandSideExpressionProductionsSpec extends ProductionSpec {
  def subject = Grammar.leftHandSideExpression

    shouldParse("@a()")      as expression.Call(
                                  body = expression.Identifier("a"),
                                  args = Seq()
                                )

    shouldParse("@a.b")      as expression.StaticProperty(
                                  body = expression.Identifier("a"),
                                  member = "b"
                                )

    shouldParse("@a['b']")   as expression.DynamicProperty(
                                  body = expression.Identifier("a"),
                                  member = expression.StringLiteral("b")
                                )

    shouldParse("@a.b()")    as expression.Call(
                                  body = expression.StaticProperty(
                                           body = expression.Identifier("a"),
                                           member = "b"
                                         ),
                                  args = Seq()
                                )

    shouldParse("@a.b('c')") as expression.Call(
                                  body = expression.StaticProperty(
                                    body = expression.Identifier("a"),
                                    member = "b"
                                  ),
                                  args = Seq(expression.StringLiteral("c"))
                                )
}
