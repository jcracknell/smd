package smd
package dom

sealed abstract class Expression extends Node

object Expression {
  trait Unary extends Expression {
    def expr: Expression
  }

  trait Binary extends Expression {
    def lhs: Expression
    def rhs: Expression
  }

  case class Addition(lhs: Expression, rhs: Expression) extends Binary
  case class ArrayLiteral(elems: Seq[Expression]) extends Expression
  case class BitwiseAnd(lhs: Expression, rhs: Expression) extends Binary
  case class BitwiseNot(expr: Expression) extends Unary
  case class BitwiseOr(lhs: Expression, rhs: Expression) extends Binary
  case class BitwiseXor(lhs: Expression, rhs: Expression) extends Binary
  case class BooleanLiteral(value: Boolean) extends Expression
  case class Call(body: Expression, args: Seq[Expression]) extends Expression
  case class Conditional(cond: Expression, trueExpr: Expression, falseExpr: Expression) extends Expression
  case class Delete(expr: Expression) extends Unary
  case class Division(lhs: Expression, rhs: Expression) extends Binary
  case class DocumentLiteral(doc: MarkDown.Document)
  case class DynamicProperty(body: Expression, member: Expression) extends Expression
  case class Elided() extends Expression
  case class Equals(lhs: Expression, rhs: Expression) extends Binary
  case class GreaterThan(lhs: Expression, rhs: Expression) extends Binary
  case class GreaterThanOrEqualTo(lhs: Expression, rhs: Expression) extends Binary
  case class Identifier(name: String) extends Expression
  case class In(lhs: Expression, rhs: Expression) extends Binary
  case class InstanceOf(expr: Expression) extends Unary
  case class LeftShift(lhs: Expression, rhs: Expression) extends Binary
  case class LessThan(lhs: Expression, rhs: Expression) extends Binary
  case class LessThanOrEqualTo(lhs: Expression, rhs: Expression) extends Binary
  case class LogicalAnd(lhs: Expression, rhs: Expression) extends Binary
  case class LogicalNot(expr: Expression) extends Unary
  case class LogicalOr(lhs: Expression, rhs: Expression) extends Binary
  case class Modulo(lhs: Expression, rhs: Expression) extends Binary
  case class Multiplication(lhs: Expression, rhs: Expression) extends Binary
  case class Negative(expr: Expression) extends Unary
  case class NotEquals(lhs: Expression, rhs: Expression) extends Binary
  case class NullLiteral() extends Expression
  case class NumericLiteral(value: Double) extends Expression
  case class ObjectLiteral(props: Seq[(String, Expression)]) extends Expression
  case class Positive(expr: Expression) extends Unary
  case class PostfixDecrement(expr: Expression) extends Unary
  case class PostfixIncrement(expr: Expression) extends Unary
  case class PrefixDecrement(expr: Expression) extends Unary
  case class PrefixIncrement(expr: Expression) extends Unary
  case class RightShift(lhs: Expression, rhs: Expression) extends Binary
  case class StaticProperty(body: Expression, member: String) extends Expression
  case class StrictEquals(lhs: Expression, rhs: Expression) extends Binary
  case class StrictNotEquals(lhs: Expression, rhs: Expression) extends Binary
  case class StringLiteral(value: String) extends Expression
  case class Subtraction(lhs: Expression, rhs: Expression) extends Binary
  case class Typeof(expr: Expression) extends Unary
  case class UnsignedRightShift(lhs: Expression, rhs: Expression) extends Binary
  case class Void(expr: Expression) extends Unary
}
