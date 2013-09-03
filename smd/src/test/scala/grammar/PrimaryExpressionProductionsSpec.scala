package smd
package grammar

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FunSpec
import smd.dom.Expression

class PrimaryExpressionProductionsSpec extends FunSpec with ShouldMatchers {
  Map(
    "[]"              -> Expression.ArrayLiteral(Seq()),
    "['a']"           -> Expression.ArrayLiteral(Seq(Expression.StringLiteral("a"))),
    "['a',42]"        -> Expression.ArrayLiteral(Seq(Expression.StringLiteral("a"), Expression.NumericLiteral(42d))),
    "[,]"             -> Expression.ArrayLiteral(Seq()),
    "[,'a']"          -> Expression.ArrayLiteral(Seq(Expression.Elided(), Expression.StringLiteral("a"))),
    "[,,'a','b',,,,]" -> Expression.ArrayLiteral(Seq(
                           Expression.Elided(),
                           Expression.Elided(),
                           Expression.StringLiteral("a"),
                           Expression.StringLiteral("b")
                         )),
    "{}"              -> Expression.ObjectLiteral(Seq()),
    "{'a':42}"        -> Expression.ObjectLiteral(Seq("a" -> Expression.NumericLiteral(42d)))
  ).foreach { case (inputString, expectedProduct) =>
    it(s"should parse ${inputString.literalEncode}") {
      val result = Grammar.PrimaryExpression.parse(inputString)

      if(result.failed) fail(s"Parsing input string ${inputString.literalEncode} failed.")

      result.product should be (expectedProduct)
    }
  }
}
