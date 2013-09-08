package smd
package grammar

class LeftHandSideExpressionProductionsSpec extends ProductionSpec {
  def subject = Grammar.LeftHandSideExpression

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
                                  member = expression.QuotedStringLiteral("b")
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
                                  args = Seq(expression.QuotedStringLiteral("c"))
                                )
}
