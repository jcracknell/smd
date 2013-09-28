package smd
package expression

sealed abstract class Expression

trait Unary {
  def expr: Expression
}

trait Binary {
  def lhs: Expression
  def rhs: Expression
}

case class Addition(lhs: Expression, rhs: Expression) extends Expression with Binary
case class ArrayLiteral(elems: Seq[Expression]) extends Expression
case class BitwiseAnd(lhs: Expression, rhs: Expression) extends Expression with Binary
case class BitwiseNot(expr: Expression) extends Expression with Unary
case class BitwiseOr(lhs: Expression, rhs: Expression) extends Expression with Binary
case class BitwiseXOr(lhs: Expression, rhs: Expression) extends Expression with Binary
case class BooleanLiteral(value: Boolean) extends Expression
case class Call(body: Expression, args: Seq[Expression]) extends Expression
case class Conditional(cond: Expression, trueExpr: Expression, falseExpr: Expression) extends Expression
case class Delete(expr: Expression) extends Expression with Unary
case class Division(lhs: Expression, rhs: Expression) extends Expression with Binary
case class DocumentLiteral(doc: markdown.Document)
case class DynamicProperty(body: Expression, member: Expression) extends Expression
case class Elided() extends Expression
case class Equals(lhs: Expression, rhs: Expression) extends Expression with Binary
case class GreaterThan(lhs: Expression, rhs: Expression) extends Expression with Binary
case class GreaterThanOrEqualTo(lhs: Expression, rhs: Expression) extends Expression with Binary
case class Identifier(name: String) extends Expression
case class In(lhs: Expression, rhs: Expression) extends Expression with Binary
case class InstanceOf(lhs: Expression, rhs: Expression) extends Expression with Binary
case class LeftShift(lhs: Expression, rhs: Expression) extends Expression with Binary
case class LessThan(lhs: Expression, rhs: Expression) extends Expression with Binary
case class LessThanOrEqualTo(lhs: Expression, rhs: Expression) extends Expression with Binary
case class LogicalAnd(lhs: Expression, rhs: Expression) extends Expression with Binary
case class LogicalNot(expr: Expression) extends Expression with Unary
case class LogicalOr(lhs: Expression, rhs: Expression) extends Expression with Binary
case class Modulo(lhs: Expression, rhs: Expression) extends Expression with Binary
case class Multiplication(lhs: Expression, rhs: Expression) extends Expression with Binary
case class Negative(expr: Expression) extends Expression with Unary
case class NotEquals(lhs: Expression, rhs: Expression) extends Expression with Binary
case class NullLiteral() extends Expression
case class NumericLiteral(value: Double) extends Expression

case class ObjectLiteral(props: Seq[(String, Expression)]) extends Expression

object ObjectLiteral {
  def apply(p0: (String, Expression), ps: (String, Expression)*): ObjectLiteral =
    apply(p0 +: ps)
}

case class Positive(expr: Expression) extends Expression with Unary
case class PostfixDecrement(expr: Expression) extends Expression with Unary
case class PostfixIncrement(expr: Expression) extends Expression with Unary
case class PrefixDecrement(expr: Expression) extends Expression with Unary
case class PrefixIncrement(expr: Expression) extends Expression with Unary
case class RightShift(lhs: Expression, rhs: Expression) extends Expression with Binary
case class StaticProperty(body: Expression, member: String) extends Expression
case class StrictEquals(lhs: Expression, rhs: Expression) extends Expression with Binary
case class StrictNotEquals(lhs: Expression, rhs: Expression) extends Expression with Binary
case class Subtraction(lhs: Expression, rhs: Expression) extends Expression with Binary
case class TypeOf(expr: Expression) extends Expression with Unary
case class UnsignedRightShift(lhs: Expression, rhs: Expression) extends Expression with Binary
case class Void(expr: Expression) extends Expression with Unary

sealed abstract class StringLikeLiteral extends Expression {
  def value: String
}

case class StringLiteral(value: String) extends StringLikeLiteral
case class VerbatimLiteral(value: String) extends StringLikeLiteral
case class IriLiteral(value: String) extends StringLikeLiteral
