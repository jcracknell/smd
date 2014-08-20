package smd
package grammar

import smd.dom._
import smd.parsing.ParsingScenarios

class LeftHandSideExpressionProductionsSpec extends ParsingScenarios {
  import Grammar.leftHandSideExpression

  parsing("@a()") as leftHandSideExpression should produce (
    Application(
      body = Identifier("a"),
      args = Seq()
    )
  )

  parsing("@a.b") as leftHandSideExpression should produce (
    Member(
      body = Identifier("a"),
      name = "b"
    )
  )

  parsing("@a.b()") as leftHandSideExpression should produce (
    Application(
      body = Member(
               body = Identifier("a"),
               name = "b"
             ),
      args = Seq()
    )
  )

  parsing("@a.b('c')") as leftHandSideExpression should produce (
    Application(
      body = Member(
        body = Identifier("a"),
        name = "b"
      ),
      args = Seq(Argument(None, StringLiteral("c")))
    )
  )

  parsing("@some(foo bar)") as leftHandSideExpression should produce (
    Application(Identifier("some"), Seq(IriLiteral("foo"), IriLiteral("bar")))
  )

  parsing("@some(foo, bar)") as leftHandSideExpression should produce (
    Application(Identifier("some"), Seq(IriLiteral("foo"), IriLiteral("bar")))
  )

  parsing("@some(foo bar baz)") as leftHandSideExpression should produce (
    Application(Identifier("some"), Seq(IriLiteral("foo"), IriLiteral("bar"), IriLiteral("baz")))
  )

  parsing("@some(foo ,bar)") as leftHandSideExpression should produce (
    Application(Identifier("some"), Seq(IriLiteral("foo"), IriLiteral("bar")))
  )

  parsing("@some(foo,bar)") as leftHandSideExpression should produce (
    Application(Identifier("some"), Seq(IriLiteral("foo,bar")))
  )

  parsing("@some(foo = bar)") as leftHandSideExpression should produce (
    Application(Identifier("some"), Seq("foo" -> IriLiteral("bar")))
  )

  parsing("@some(foo=bar)") as leftHandSideExpression should produce (
    Application(Identifier("some"), Seq("foo" -> IriLiteral("bar")))
  )

  parsing("""@code(lang=scala, ```def foo(): String = { "bar" }```)""") as leftHandSideExpression should produce (
    Application(Identifier("code"), Seq(
      "lang" -> IriLiteral("scala"),
      VerbatimLiteral("""def foo(): String = { "bar" }""")
    ))
  )
}
