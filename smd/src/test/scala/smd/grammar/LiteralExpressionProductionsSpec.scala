package smd
package grammar

class LiteralExpressionProductionsSpec extends ProductionSpec {
  def subject = Grammar.LiteralExpression

  describe("NumericLiteralExpression") {
    shouldParse("0")      as expression.NumericLiteral(0d)
    shouldParse(".0123")  as expression.NumericLiteral(0.0123d)
    shouldParse("12.345") as expression.NumericLiteral(12.345d)
    shouldParse("12e1")   as expression.NumericLiteral(120d)
    shouldParse("12E2")   as expression.NumericLiteral(1200d)
    shouldParse("0x1234") as expression.NumericLiteral(0x1234.toDouble)
  }
  describe("QuotedStringLiteralExpression") {
    shouldParse("\"\"")         as expression.QuotedStringLiteral("")
    shouldParse("''")           as expression.QuotedStringLiteral("")
    shouldParse("``")           as expression.VerbatimStringLiteral("")
    shouldParse("\"a\"")        as expression.QuotedStringLiteral("a")
    shouldParse("'a'")          as expression.QuotedStringLiteral("a")
    shouldParse("`a`")          as expression.VerbatimStringLiteral("a")
    shouldParse("````````````a````````````") as expression.VerbatimStringLiteral("a")
    shouldParse("\"\\0a\"")     as expression.QuotedStringLiteral("\0a")
    shouldParse("`\\0a`")       as expression.VerbatimStringLiteral("\\0a")
    shouldParse("\"\\000a\"")   as expression.QuotedStringLiteral("\0a")
    shouldParse("`\\000a`")     as expression.VerbatimStringLiteral("\\000a")
    shouldParse("\"\\x00a\"")   as expression.QuotedStringLiteral("\0a")
    shouldParse("`\\x00a`")     as expression.VerbatimStringLiteral("\\x00a")
    shouldParse("\"\\x4a\"")    as expression.QuotedStringLiteral("J")
    shouldParse("`\\x4a`")      as expression.VerbatimStringLiteral("\\x4a")
    shouldParse("\"\\u0000a\"") as expression.QuotedStringLiteral("\0a")
    shouldParse("`\\u0000a`")   as expression.VerbatimStringLiteral("\\u0000a")
    shouldParse("\"\\u004a\"")  as expression.QuotedStringLiteral("J")
    shouldParse("`\\u004a`")    as expression.VerbatimStringLiteral("\\u004a")
  }
}