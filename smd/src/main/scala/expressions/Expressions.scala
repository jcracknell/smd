package smd
package expressions

import smd.nodes.DocumentNode

sealed trait Expression

sealed trait UnaryExpression extends Expression {
  def expr: Expression
}

sealed trait BinaryExpression extends Expression {
  def lhs: Expression
  def rhs: Expression
}

case class AdditionExpression(lhs: Expression, rhs: Expression) extends BinaryExpression
case class ArrayLiteralExpression(elems: Seq[Expression]) extends Expression
case class BitwiseAndExpression(lhs: Expression, rhs: Expression) extends BinaryExpression
case class BitwiseNotExpression(lhs: Expression, rhs: Expression) extends BinaryExpression
case class BitwiseOrExpression(lhs: Expression, rhs: Expression) extends BinaryExpression
case class BitwiseXorExpression(lhs: Expression, rhs: Expression) extends BinaryExpression
case class BooleanLiteralExpression(value: Boolean) extends Expression
case class CallExpression(body: Expression, args: Seq[Expression]) extends Expression
case class ConditionalExpression(cond: Expression, trueExpr: Expression, falseExpr: Expression) extends Expression
case class DeleteExpression(expr: Expression) extends UnaryExpression
case class DivisionExpression(lhs: Expression, rhs: Expression) extends BinaryExpression
case class DocumentLiteralExpression(doc: DocumentNode)
case class DynamicPropertyExpression(body: Expression, member: Expression) extends Expression
case class ElidedExpression() extends Expression
case class EqualsExpression(lhs: Expression, rhs: Expression) extends BinaryExpression
case class GreaterThanExpression(lhs: Expression, rhs: Expression) extends BinaryExpression
case class GreaterThanOrEqualToExpression(lhs: Expression, rhs: Expression) extends BinaryExpression
case class IdentifierExpression(name: String) extends Expression
case class InExpression(lhs: Expression, rhs: Expression) extends BinaryExpression
case class InstanceOfExpression(expr: Expression) extends UnaryExpression
case class LeftShiftExpression(lhs: Expression, rhs: Expression) extends BinaryExpression
case class LessThanExpression(lhs: Expression, rhs: Expression) extends BinaryExpression
case class LessThanOrEqualToExpression(lhs: Expression, rhs: Expression) extends BinaryExpression
case class LogicalAndExpression(lhs: Expression, rhs: Expression) extends BinaryExpression
case class LogicalNotExpression(expr: Expression) extends UnaryExpression
case class LogicalOrExpression(lhs: Expression, rhs: Expression) extends BinaryExpression
case class ModuloExpression(lhs: Expression, rhs: Expression) extends BinaryExpression
case class MultiplicationExpression(lhs: Expression, rhs: Expression) extends BinaryExpression
case class NegativeExpression(expr: Expression) extends UnaryExpression
case class NotEqualsExpression(lhs: Expression, rhs: Expression) extends BinaryExpression
case class NullLiteralExpression() extends Expression
case class NumericLiteralExpression(value: Double) extends Expression
case class ObjectLiteralExpression(props: Seq[(String, Expression)]) extends Expression
case class PositiveExpression(expr: Expression) extends UnaryExpression
case class PostfixDecrementExpression(expr: Expression) extends UnaryExpression
case class PostfixIncrementExpression(expr: Expression) extends UnaryExpression
case class PrefixDecrementExpression(expr: Expression) extends UnaryExpression
case class PrefixIncrementExpression(expr: Expression) extends UnaryExpression
case class RightShiftExpression(lhs: Expression, rhs: Expression) extends BinaryExpression
case class StaticPropertyExpression(body: Expression, member: String) extends Expression
case class StrictEqualsExpression(lhs: Expression, rhs: Expression) extends BinaryExpression
case class StrictNotEqualsExpression(lhs: Expression, rhs: Expression) extends BinaryExpression
case class StringLiteralExpression(value: String) extends Expression
case class SubtractionExpression(lhs: Expression, rhs: Expression) extends BinaryExpression
case class TypeofExpression(expr: Expression) extends UnaryExpression
case class UnsignedRightShiftExpression(lhs: Expression, rhs: Expression) extends BinaryExpression
case class VoidExpression(expr: Expression) extends UnaryExpression
