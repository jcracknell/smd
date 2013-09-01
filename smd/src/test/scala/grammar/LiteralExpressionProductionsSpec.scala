package smd
package grammar

import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers
import smd.expressions._
import smd.parsing.ParsingContext

class LiteralExpressionProductionsSpec extends FunSpec with ShouldMatchers {
  describe("NumericLiteralExpression") {
    Map(
      "0"      -> 0d,
      ".0123"  -> 0.0123d,
      "12.345" -> 12.345d,
      "12e1"   -> 120d,
      "12E2"   -> 1200d,
      "0x1234" -> 0x1234.toDouble
    ).mapValues(NumericLiteralExpression(_))
    .foreach { case (input, product) =>
      it(s"should parse ${input.literalEncode} as ${product.toString}") {
        val result = Grammar.NumericLiteralExpression.parse(input)
        result.product should be (product)
      }
    }
  }
  describe("StringLiteralCharacter") {
    it("should parse a regular character") {
      Grammar.StringLiteralCharacter.parse("a").succeeded should be (true)
    }
    it("should parse an escaped newline character") {
      Grammar.StringLiteralCharacter.parse("\\n").succeeded should be (true)
    }
    it("should parse a unicode escape sequence") {
      Grammar.StringLiteralCharacter.parse("\\u0300").succeeded should be (true)
    }
  }
  describe("StringLiteralExpression") {
    it("should parse an empty double-quoted string literal") {
      val product =  Grammar.StringLiteralExpression.parse(ParsingContext("\"\"")).product
      (product) should be (StringLiteralExpression(""))
    }
    it("should parse a non-empty double-quoted string literal") {
      val product =  Grammar.StringLiteralExpression.parse(ParsingContext("\"a\"")).product
      (product) should be (StringLiteralExpression("a"))
    }
    it("should parse an empty single-quoted string literal") {
      val product = Grammar.StringLiteralExpression.parse(ParsingContext("''")).product
      (product) should be (StringLiteralExpression(""))
    }
    it("should parse a non-empty single-quoted string literal") {
      val product =  Grammar.StringLiteralExpression.parse(ParsingContext("'a'")).product
      (product) should be (StringLiteralExpression("a"))
    }
  }
}
