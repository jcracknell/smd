package smd
package grammar

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FunSpec
import smd.dom.Expression

class LeftHandSideExpressionProductionsSpec extends FunSpec with ShouldMatchers {
  Map(
    "a()"      -> Expression.Call(
                    body = Expression.Identifier("a"),
                    args = Seq()
                  ),
    "a.b"      -> Expression.StaticProperty(
                    body = Expression.Identifier("a"),
                    member = "b"
                  ),
    "a['b']"   -> Expression.DynamicProperty(
                    body = Expression.Identifier("a"),
                    member = Expression.StringLiteral("b")
                  ),
    "a.b()"    -> Expression.Call(
                    body = Expression.StaticProperty(
                             body = Expression.Identifier("a"),
                             member = "b"
                           ),
                    args = Seq()
                  ),
    "a.b('c')" -> Expression.Call(
                    body = Expression.StaticProperty(
                      body = Expression.Identifier("a"),
                      member = "b"
                    ),
                    args = Seq(Expression.StringLiteral("c"))
                  )
  ).foreach { case (inputStr, expectedProduct) =>
    it(s"should parse ${inputStr.toString}") {
      val result = Grammar.LeftHandSideExpression.parse(inputStr)

      if(result.failed)
        fail(s"parsing of ${inputStr.toString} failed")

      result.product should be (expectedProduct)
    }
  }
}
