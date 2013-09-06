package smd
package grammar

import smd.dom.Expression

class BinaryExpressionProductionsSpec extends ProductionSpec {
  def subject = Grammar.LogicalOrExpression

  describe("LogicalOrExpression") {
    shouldParse("@a || @b") as Expression.LogicalOr(Expression.Identifier("a"), Expression.Identifier("b"))
  }
  describe("LogicalAndExpression") {
    shouldParse("true && false") as Expression.LogicalAnd(Expression.BooleanLiteral(true), Expression.BooleanLiteral(false))
  }
  describe("BitwiseOrExpression") {
    shouldParse("@a.foo() | 42") as Expression.BitwiseOr(
                                      Expression.Call(
                                        Expression.StaticProperty(Expression.Identifier("a"), "foo"),
                                        Seq()
                                      ),
                                      Expression.NumericLiteral(42d)
                                    )
  }
  describe("BitwiseXOrExpression") {
    shouldParse("42 ^ 7") as Expression.BitwiseXOr(Expression.NumericLiteral(42), Expression.NumericLiteral(7))
  }
  describe("BitwiseAndExpression") {
    shouldParse("42 & 7") as Expression.BitwiseAnd(Expression.NumericLiteral(42), Expression.NumericLiteral(7))
  }
  describe("EqualityExpression") {
    shouldParse("42 === 7") as Expression.StrictEquals(Expression.NumericLiteral(42), Expression.NumericLiteral(7))
    shouldParse("42 == 7")  as Expression.Equals(Expression.NumericLiteral(42), Expression.NumericLiteral(7))
    shouldParse("42 !== 7") as Expression.StrictNotEquals(Expression.NumericLiteral(42), Expression.NumericLiteral(7))
    shouldParse("42 != 7")  as Expression.NotEquals(Expression.NumericLiteral(42), Expression.NumericLiteral(7))
  }
  describe("RelationalExpression") {
    shouldParse("42 < 7")          as Expression.LessThan(Expression.NumericLiteral(42), Expression.NumericLiteral(7))
    shouldParse("42 <= 7")         as Expression.LessThanOrEqualTo(Expression.NumericLiteral(42), Expression.NumericLiteral(7))
    shouldParse("42 > 7")          as Expression.GreaterThan(Expression.NumericLiteral(42), Expression.NumericLiteral(7))
    shouldParse("42 >= 7")         as Expression.GreaterThanOrEqualTo(Expression.NumericLiteral(42), Expression.NumericLiteral(7))
    shouldParse("42 in 7")         as Expression.In(Expression.NumericLiteral(42), Expression.NumericLiteral(7))
    shouldParse("42 instanceof 7") as Expression.InstanceOf(Expression.NumericLiteral(42), Expression.NumericLiteral(7))
  }
  describe("ShiftExpression") {
    shouldParse("42 >> 7")  as Expression.RightShift(Expression.NumericLiteral(42), Expression.NumericLiteral(7))
    shouldParse("42 >>> 7") as Expression.UnsignedRightShift(Expression.NumericLiteral(42), Expression.NumericLiteral(7))
    shouldParse("42 << 7")  as Expression.LeftShift(Expression.NumericLiteral(42), Expression.NumericLiteral(7))
  }
  describe("AdditiveExpression") {
    shouldParse("42 + 7")  as Expression.Addition(Expression.NumericLiteral(42), Expression.NumericLiteral(7))
    shouldParse("42 - 7")  as Expression.Subtraction(Expression.NumericLiteral(42), Expression.NumericLiteral(7))
  }
  describe("MultiplicativeExpression") {
    shouldParse("42 * 7") as Expression.Multiplication(Expression.NumericLiteral(42), Expression.NumericLiteral(7))
    shouldParse("42 / 7") as Expression.Division(Expression.NumericLiteral(42), Expression.NumericLiteral(7))
    shouldParse("42 % 7") as Expression.Modulo(Expression.NumericLiteral(42), Expression.NumericLiteral(7))
  }
}
