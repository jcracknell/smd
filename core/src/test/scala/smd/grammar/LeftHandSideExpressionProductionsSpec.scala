package smd
package grammar

import smd.dom._
import smd.parsing.ParsingScenarios

class LeftHandSideExpressionProductionsSpec extends ParsingScenarios {
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
      args = Seq(Argument(None, StringLiteral("c")))
    )
  )

  parsing("@some(foo = bar)") as leftHandSideExpression should produce (
    Call(Identifier("some"), Seq("foo" -> IriLiteral("bar")))
  )

  parsing("@some(foo=bar)") as leftHandSideExpression should produce (
    Call(Identifier("some"), Seq("foo" -> IriLiteral("bar")))
  )

  parsing("""@code(lang=scala, ```def foo(): String = { "bar" }```)""") as leftHandSideExpression should produce (
    Call(Identifier("code"), Seq(
      "lang" -> IriLiteral("scala"),
      VerbatimLiteral("""def foo(): String = { "bar" }""")
    ))
  )
}
