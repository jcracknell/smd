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
    shouldParse("''")           as expression.QuotedStringLiteral("")
    shouldParse("\"\"")         as expression.QuotedStringLiteral("")
    shouldParse("``")           as expression.VerbatimStringLiteral("")
    shouldParse("'a'")          as expression.QuotedStringLiteral("a")
    shouldParse("\"a\"")        as expression.QuotedStringLiteral("a")
    shouldParse("`a`")          as expression.VerbatimStringLiteral("a")
    shouldParse("````````````a````````````") as expression.VerbatimStringLiteral("a")
    shouldParse("'\\0z'")       as expression.QuotedStringLiteral("0z")
    shouldParse("`\\0z`")       as expression.VerbatimStringLiteral("\\0z")
    shouldParse("'\\x00z'")     as expression.QuotedStringLiteral("\0z")
    shouldParse("`\\x00z`")     as expression.VerbatimStringLiteral("\\x00z")
    shouldParse("'\\x4a'")      as expression.QuotedStringLiteral("J")
    shouldParse("`\\x4z`")      as expression.VerbatimStringLiteral("\\x4z")
    shouldParse("'\\u0000z'")   as expression.QuotedStringLiteral("\0z")
    shouldParse("'\\x0000z'")   as expression.QuotedStringLiteral("\0z")
    shouldParse("'\\#u0000z'")  as expression.QuotedStringLiteral("\0z")
    shouldParse("'\\#x0000z'")  as expression.QuotedStringLiteral("\0z")
    shouldParse("'\\u0000;z'")  as expression.QuotedStringLiteral("\0z")
    shouldParse("'\\x0000;z'")  as expression.QuotedStringLiteral("\0z")
    shouldParse("`\\u0000z`")   as expression.VerbatimStringLiteral("\\u0000z")
    shouldParse("'\\u004a'")    as expression.QuotedStringLiteral("J")
    shouldParse("'\\#u4a'")     as expression.QuotedStringLiteral("J")
    shouldParse("'\\#u4a;'")    as expression.QuotedStringLiteral("J")
    shouldParse("'\\u4a;'")     as expression.QuotedStringLiteral("J")
    shouldParse("`\\u004z`")    as expression.VerbatimStringLiteral("\\u004z")
    shouldParse("'\\eacutez'")  as expression.QuotedStringLiteral("eacutez")
    shouldParse("'\\eacute;z'") as expression.QuotedStringLiteral("éz")
  }
}
