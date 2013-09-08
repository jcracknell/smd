package smd
package grammar

class BinaryexpressionsProductionsSpec extends ProductionSpec {
  def subject = Grammar.LogicalOrExpression

  describe("LogicalOrExpression") {
    shouldParse("@a || @b") as expression.LogicalOr(expression.Identifier("a"), expression.Identifier("b"))
  }
  describe("LogicalAndExpression") {
    shouldParse("true && false") as expression.LogicalAnd(expression.BooleanLiteral(true), expression.BooleanLiteral(false))
  }
  describe("BitwiseOrExpression") {
    shouldParse("@a.foo() | 42") as expression.BitwiseOr(
                                      expression.Call(
                                        expression.StaticProperty(expression.Identifier("a"), "foo"),
                                        Seq()
                                      ),
                                      expression.NumericLiteral(42d)
                                    )
  }
  describe("BitwiseXOrExpression") {
    shouldParse("42 ^ 7") as expression.BitwiseXOr(expression.NumericLiteral(42), expression.NumericLiteral(7))
  }
  describe("BitwiseAndExpression") {
    shouldParse("42 & 7") as expression.BitwiseAnd(expression.NumericLiteral(42), expression.NumericLiteral(7))
  }
  describe("EqualityExpression") {
    shouldParse("42 === 7") as expression.StrictEquals(expression.NumericLiteral(42), expression.NumericLiteral(7))
    shouldParse("42 == 7")  as expression.Equals(expression.NumericLiteral(42), expression.NumericLiteral(7))
    shouldParse("42 !== 7") as expression.StrictNotEquals(expression.NumericLiteral(42), expression.NumericLiteral(7))
    shouldParse("42 != 7")  as expression.NotEquals(expression.NumericLiteral(42), expression.NumericLiteral(7))
  }
  describe("RelationalExpression") {
    shouldParse("42 < 7")          as expression.LessThan(expression.NumericLiteral(42), expression.NumericLiteral(7))
    shouldParse("42 <= 7")         as expression.LessThanOrEqualTo(expression.NumericLiteral(42), expression.NumericLiteral(7))
    shouldParse("42 > 7")          as expression.GreaterThan(expression.NumericLiteral(42), expression.NumericLiteral(7))
    shouldParse("42 >= 7")         as expression.GreaterThanOrEqualTo(expression.NumericLiteral(42), expression.NumericLiteral(7))
    shouldParse("42 in 7")         as expression.In(expression.NumericLiteral(42), expression.NumericLiteral(7))
    shouldParse("42 instanceof 7") as expression.InstanceOf(expression.NumericLiteral(42), expression.NumericLiteral(7))
  }
  describe("ShiftExpression") {
    shouldParse("42 >> 7")  as expression.RightShift(expression.NumericLiteral(42), expression.NumericLiteral(7))
    shouldParse("42 >>> 7") as expression.UnsignedRightShift(expression.NumericLiteral(42), expression.NumericLiteral(7))
    shouldParse("42 << 7")  as expression.LeftShift(expression.NumericLiteral(42), expression.NumericLiteral(7))
  }
  describe("AdditiveExpression") {
    shouldParse("42 + 7")  as expression.Addition(expression.NumericLiteral(42), expression.NumericLiteral(7))
    shouldParse("42 - 7")  as expression.Subtraction(expression.NumericLiteral(42), expression.NumericLiteral(7))
  }
  describe("MultiplicativeExpression") {
    shouldParse("42 * 7") as expression.Multiplication(expression.NumericLiteral(42), expression.NumericLiteral(7))
    shouldParse("42 / 7") as expression.Division(expression.NumericLiteral(42), expression.NumericLiteral(7))
    shouldParse("42 % 7") as expression.Modulo(expression.NumericLiteral(42), expression.NumericLiteral(7))
  }
}
