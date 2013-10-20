package smd
package dom

sealed abstract class Node

sealed abstract class Markdown extends Node
sealed abstract class Expression extends Node

//region Markdown

sealed abstract class Block extends Markdown

sealed abstract class Inline extends Markdown


trait Composite[+A] {
  def children: Seq[A]
}

//region Block

case class Blockquote(children: Seq[Block]) extends Block with Composite[Block]

case class ExpressionBlock(expr: Expression) extends Block

case class Heading(level: Int, children: Seq[Inline]) extends Block with Composite[Inline]

case class Paragraph(children: Seq[Inline]) extends Block with Composite[Inline]

case class Reference(ref: ReferenceId, args: Seq[Expression]) extends Block

sealed abstract class List extends Block {
  type Item
  def items: Seq[Item]
}

//region Lists

sealed abstract class DefinitionList extends List {
  type Item <: DefinitionList.Item[_ <: Markdown]
}

object DefinitionList {
  case class Loose(items: Seq[DefinitionList.Item[Block]]) extends DefinitionList {
    type Item = DefinitionList.Item[Block]
  }

  case class Tight(items: Seq[DefinitionList.Item[Inline]]) extends DefinitionList {
    type Item = DefinitionList.Item[Inline]
  }

  case class Item[+A](term: Term[A], defs: Definition[A]*)
  case class Term[+A](children: Seq[A]) extends Composite[A]
  case class Definition[+A](children: Seq[A]) extends Composite[A]
}

sealed abstract class OrderedList extends List {
  type Item <: OrderedList.Item[_ <: Markdown]
  def counterStyle: OrderedList.CounterStyle
}

object OrderedList {
  case class Tight(items: Seq[OrderedList.Item[Inline]], counterStyle: CounterStyle) extends OrderedList {
    type Item = OrderedList.Item[Inline]
  }

  case class Loose(items: Seq[OrderedList.Item[Block]], counterStyle: CounterStyle) extends OrderedList {
    type Item = OrderedList.Item[Block]
  }

  case class Item[+A](children: Seq[A]) extends Composite[A]

  sealed abstract class CounterStyle
  object CounterStyle {
    case object Arabic extends CounterStyle
    case object LowerAlpha extends CounterStyle
    case object UpperAlpha extends CounterStyle
    case object LowerRoman extends CounterStyle
    case object UpperRoman extends CounterStyle
  }
}

sealed abstract class UnorderedList extends List {
  type Item <: UnorderedList.Item[_ <: Markdown]
}

object UnorderedList {
  case class Loose(items: Seq[UnorderedList.Item[Block]]) extends UnorderedList {
    type Item = UnorderedList.Item[Block]
  }

  case class Tight(items: Seq[UnorderedList.Item[Inline]]) extends UnorderedList {
    type Item = UnorderedList.Item[Inline]
  }

  case class Item[+A](children: Seq[A]) extends Composite[A]
}

//endregion

//endregion

//region Inline

sealed abstract class Atomic extends Inline
sealed abstract class Span extends Inline with Composite[Inline]

//region Atomic

case class AutoLink(uri: String) extends Atomic
case class Code(value: String) extends Atomic
case class InlineExpression(expr: Expression) extends Atomic
case class LineBreak() extends Atomic
case class Space() extends Atomic
case class Symbol(value: String) extends Atomic
case class Text(value: String) extends Atomic
case class Entity(codePoints: Seq[Int]) extends Atomic

//endregion

//region Span

case class Emphasis(children: Seq[Inline]) extends Span
case class Link(children: Seq[Inline], ref: Option[ReferenceId], args: Seq[Expression]) extends Span
case class Quoted(children: Seq[Inline], kind: Quoted.QuoteKind) extends Span

object Quoted {
  sealed abstract class QuoteKind
  object QuoteKind {
    case object Double extends QuoteKind
    case object Single extends QuoteKind
  }
}

case class Strong(children: Seq[Inline]) extends Span

//endregion

//endregion

//endregion

//region Expressions

trait Unary { self: Expression =>
  def expr: Expression
}

trait Binary { self: Expression =>
  def lhs: Expression
  def rhs: Expression
}

//region Literals

case class ArrayLiteral(elems: Seq[Expression]) extends Expression
case class BooleanLiteral(value: Boolean) extends Expression
case class DocumentLiteral(doc: Document) extends Expression
case class NullLiteral() extends Expression
case class NumericLiteral(value: Double) extends Expression

case class ObjectLiteral(props: Seq[(String, Expression)]) extends Expression

object ObjectLiteral {
  def apply(): ObjectLiteral = apply(Seq())
  def apply(p0: (String, Expression), ps: (String, Expression)*): ObjectLiteral = apply(p0 +: ps)
}

sealed abstract class StringLikeLiteral extends Expression {
  def value: String
}

case class StringLiteral(value: String) extends StringLikeLiteral
case class VerbatimLiteral(value: String) extends StringLikeLiteral
case class IriLiteral(value: String) extends StringLikeLiteral

//endregion

case class Addition(lhs: Expression, rhs: Expression) extends Expression with Binary
case class BitwiseAnd(lhs: Expression, rhs: Expression) extends Expression with Binary
case class BitwiseNot(expr: Expression) extends Expression with Unary
case class BitwiseOr(lhs: Expression, rhs: Expression) extends Expression with Binary
case class BitwiseXOr(lhs: Expression, rhs: Expression) extends Expression with Binary
case class Call(body: Expression, args: Seq[Expression]) extends Expression
case class Conditional(cond: Expression, trueExpr: Expression, falseExpr: Expression) extends Expression
case class Delete(expr: Expression) extends Expression with Unary
case class Division(lhs: Expression, rhs: Expression) extends Expression with Binary
case class DynamicProperty(body: Expression, member: Expression) extends Expression
case class Elided() extends Expression
case class Equals(lhs: Expression, rhs: Expression) extends Expression with Binary
case class GreaterThan(lhs: Expression, rhs: Expression) extends Expression with Binary
case class GreaterThanOrEqualTo(lhs: Expression, rhs: Expression) extends Expression with Binary
case class Identifier(name: String) extends Expression
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

//endregion
