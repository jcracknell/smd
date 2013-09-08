package smd
package grammar

import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers
import smd.parsing.ParsingContext
import smd.dom.Expression

class LiteralExpressionProductionsSpec extends ProductionSpec {
  def subject = Grammar.LiteralExpression

  describe("NumericLiteralExpression") {
    shouldParse("0")      as Expression.NumericLiteral(0d)
    shouldParse(".0123")  as Expression.NumericLiteral(0.0123d)
    shouldParse("12.345") as Expression.NumericLiteral(12.345d)
    shouldParse("12e1")   as Expression.NumericLiteral(120d)
    shouldParse("12E2")   as Expression.NumericLiteral(1200d)
    shouldParse("0x1234") as Expression.NumericLiteral(0x1234.toDouble)
  }
  describe("StringLiteralExpression") {
    shouldParse("\"\"")         as Expression.StringLiteral("")
    shouldParse("''")           as Expression.StringLiteral("")
    shouldParse("``")           as Expression.StringLiteral("")
    shouldParse("\"a\"")        as Expression.StringLiteral("a")
    shouldParse("'a'")          as Expression.StringLiteral("a")
    shouldParse("`a`")          as Expression.StringLiteral("a")
    shouldParse("````````````a````````````") as Expression.StringLiteral("a")
    shouldParse("\"\\0a\"")     as Expression.StringLiteral("\0a")
    shouldParse("`\\0a`")       as Expression.StringLiteral("\\0a")
    shouldParse("\"\\000a\"")   as Expression.StringLiteral("\0a")
    shouldParse("`\\000a`")     as Expression.StringLiteral("\\000a")
    shouldParse("\"\\x00a\"")   as Expression.StringLiteral("\0a")
    shouldParse("`\\x00a`")     as Expression.StringLiteral("\\x00a")
    shouldParse("\"\\x4a\"")    as Expression.StringLiteral("J")
    shouldParse("`\\x4a`")      as Expression.StringLiteral("\\x4a")
    shouldParse("\"\\u0000a\"") as Expression.StringLiteral("\0a")
    shouldParse("`\\u0000a`")   as Expression.StringLiteral("\\u0000a")
    shouldParse("\"\\u004a\"")  as Expression.StringLiteral("J")
    shouldParse("`\\u004a`")    as Expression.StringLiteral("\\u004a")
  }
}
