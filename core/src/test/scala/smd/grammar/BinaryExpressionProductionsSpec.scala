package smd
package grammar

import smd.dom._
import smd.parsing.ParsingScenarios

class BinaryExpressionsProductionsSpec extends ParsingScenarios {
  import Grammar.logicalOrExpression

  describe("LogicalOrExpression") {
    parsing("@a || @b") as logicalOrExpression should produce (
      LogicalOr(Identifier("a"), Identifier("b"))
    )
    parsing("@a.foo() | 42") as logicalOrExpression should produce (
      LogicalOr(
        Application(Member(Identifier("a"), "foo"), Seq()),
        NumericLiteral(42d)
      )
    )
  }
  describe("LogicalAndExpression") {
    parsing("true && false") as logicalOrExpression should produce (
      LogicalAnd(BooleanLiteral(true), BooleanLiteral(false))
    )
    parsing("42 & 7") as logicalOrExpression should produce (
      LogicalAnd(NumericLiteral(42), NumericLiteral(7))
    )
  }
  describe("EqualityExpression") {
    parsing("42 === 7") as logicalOrExpression should produce (
      StrictEquals(NumericLiteral(42), NumericLiteral(7))
    )
    parsing("42 == 7")  as logicalOrExpression should produce (
      Equals(NumericLiteral(42), NumericLiteral(7))
    )
    parsing("42 !== 7") as logicalOrExpression should produce (
      StrictNotEquals(NumericLiteral(42), NumericLiteral(7))
    )
    parsing("42 != 7")  as logicalOrExpression should produce (
      NotEquals(NumericLiteral(42), NumericLiteral(7))
    )
  }
  describe("RelationalExpression") {
    parsing("42 < 7") as logicalOrExpression should produce (
      LessThan(NumericLiteral(42), NumericLiteral(7))
    )
    parsing("42 <= 7") as logicalOrExpression should produce (
      LessThanOrEqualTo(NumericLiteral(42), NumericLiteral(7))
    )
    parsing("42 > 7") as logicalOrExpression should produce (
      GreaterThan(NumericLiteral(42), NumericLiteral(7))
    )
    parsing("42 >= 7") as logicalOrExpression should produce (
      GreaterThanOrEqualTo(NumericLiteral(42), NumericLiteral(7))
    )
  }
  describe("AdditiveExpression") {
    parsing("42 + 7")  as logicalOrExpression should produce (
      Addition(NumericLiteral(42), NumericLiteral(7))
    )
    parsing("42 - 7")  as logicalOrExpression should produce (
      Subtraction(NumericLiteral(42), NumericLiteral(7))
    )
  }
  describe("MultiplicativeExpression") {
    parsing("42 * 7") as logicalOrExpression should produce (
      Multiplication(NumericLiteral(42), NumericLiteral(7))
    )
    parsing("42 / 7") as logicalOrExpression should produce (
      Division(NumericLiteral(42), NumericLiteral(7))
    )
    parsing("42 % 7") as logicalOrExpression should produce (
      Modulo(NumericLiteral(42), NumericLiteral(7))
    )
  }
  describe("ExponentiationExpression") {
    parsing("42 ^ 7") as logicalOrExpression should produce (
      Exponentiation(NumericLiteral(42), NumericLiteral(7))
    )
  }
}
