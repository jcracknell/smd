package smd
package dom

import scala.language.{higherKinds, implicitConversions}
import smd.util.NumeralSystem

case class Document(content: Seq[Block])

trait Visitable[-V[_]] {
  def accept[A](visitor: V[A]): A
}

sealed abstract class Node extends Visitable[Node.Visitor]

object Node {
  trait Visitor[+A] extends Expression.Visitor[A] with Markdown.Visitor[A]
}

sealed abstract class Markdown extends Node with Visitable[Markdown.Visitor]

object Markdown {
  trait Visitor[+A] extends Block.Visitor[A] with Inline.Visitor[A]
}

trait Composite[+A] {
  def content: Seq[A]
}

trait Referenceable {
  def refId: Option[ReferenceId]
}

sealed abstract class Expression extends Node with Visitable[Expression.Visitor]

object Expression {
  trait Visitor[+A] {
    def visit(node: ArrayLiteral): A
    def visit(node: BooleanLiteral): A
    def visit(node: DocumentLiteral): A
    def visit(node: NullLiteral): A
    def visit(node: NumericLiteral): A
    def visit(node: ObjectLiteral): A
    def visit(node: StringLiteral): A
    def visit(node: VerbatimLiteral): A
    def visit(node: IriLiteral): A
    def visit(node: Addition): A
    def visit(node: Application): A
    def visit(node: BitwiseAnd): A
    def visit(node: BitwiseNot): A
    def visit(node: BitwiseOr): A
    def visit(node: BitwiseXOr): A
    def visit(node: Conditional): A
    def visit(node: Division): A
    def visit(node: Equals): A
    def visit(node: GreaterThan): A
    def visit(node: GreaterThanOrEqualTo): A
    def visit(node: Identifier): A
    def visit(node: LeftShift): A
    def visit(node: LessThan): A
    def visit(node: LessThanOrEqualTo): A
    def visit(node: LogicalAnd): A
    def visit(node: LogicalNot): A
    def visit(node: LogicalOr): A
    def visit(node: Member): A
    def visit(node: Modulo): A
    def visit(node: Multiplication): A
    def visit(node: Negative): A
    def visit(node: NotEquals): A
    def visit(node: Positive): A
    def visit(node: RightShift): A
    def visit(node: StrictEquals): A
    def visit(node: StrictNotEquals): A
    def visit(node: Subtraction): A
    def visit(node: UnsignedRightShift): A
  }
}

//region Markdown

sealed abstract class Block extends Markdown with Visitable[Block.Visitor]

object Block {
  trait Visitor[+A] {
    def visit(node: ExpressionBlock): A
    def visit(node: Blockquote): A
    def visit(node: Heading): A
    def visit(node: Paragraph): A
    def visit(node: Reference): A
    def visit(node: DefinitionList.Loose): A
    def visit(node: DefinitionList.Tight): A
    def visit(node: OrderedList.Loose): A
    def visit(node: OrderedList.Tight): A
    def visit(node: Table): A
    def visit(node: UnorderedList.Loose): A
    def visit(node: UnorderedList.Tight): A
  }
}

sealed abstract class Inline extends Markdown with Visitable[Inline.Visitor]

object Inline {
  trait Visitor[+A] extends Atomic.Visitor[A] with Span.Visitor[A]
}

//region Block

case class Blockquote(content: Seq[Block]) extends Block with Composite[Block] {
  def accept[A](visitor: Block.Visitor[A]): A = visitor.visit(this)
}

case class ExpressionBlock(expr: Expression) extends Block {
  def accept[A](visitor: Block.Visitor[A]): A = visitor.visit(this)
}

case class Heading(level: Int, content: Seq[Inline]) extends Block with Composite[Inline] {
  def accept[A](visitor: Block.Visitor[A]): A = visitor.visit(this)
}

case class Paragraph(content: Seq[Inline]) extends Block with Composite[Inline] {
  def accept[A](visitor: Block.Visitor[A]): A = visitor.visit(this)
}

case class Reference(ref: ReferenceId, args: Seq[Argument]) extends Block {
  def accept[A](visitor: Block.Visitor[A]): A = visitor.visit(this)
}

case class Table(head: Seq[Table.Row], body: Seq[Table.Row]) extends Block {
  def accept[A](visitor: Block.Visitor[A]): A = visitor.visit(this)
}

object Table {
  case class Row(cells: Cell*)
  case class Cell(alignment: CellAlignment, span: Int, content: Seq[Inline]) extends Composite[Inline]

  sealed abstract class CellAlignment
  object CellAlignment {
    case object Left   extends CellAlignment
    case object Right  extends CellAlignment
    case object Center extends CellAlignment
  }
}

sealed abstract class List extends Block {
  type Item
  def items: Seq[Item]
}

//region Lists

sealed abstract class DefinitionList extends List {
  type Item <: DefinitionList.Item[_]
}

object DefinitionList {
  case class Term(content: Seq[Inline]) extends Composite[Inline]
  case class Definition[+A <: Markdown](content: Seq[A]) extends Composite[A]

  sealed abstract class Item[+A <: Markdown] {
    def term: Term
    def defs: Seq[Definition[A]]
  }

  case class Loose(items: Seq[DefinitionList.Loose.Item]) extends DefinitionList {
    type Item = DefinitionList.Loose.Item
    def accept[A](visitor: Block.Visitor[A]): A = visitor.visit(this)
  }

  object Loose {
    case class Item(term: Term, defs: Seq[Definition[Block]]) extends DefinitionList.Item[Block]
  }

  case class Tight(items: Seq[DefinitionList.Tight.Item]) extends DefinitionList {
    type Item = DefinitionList.Tight.Item
    def accept[A](visitor: Block.Visitor[A]): A = visitor.visit(this)
  }

  object Tight {
    case class Item(term: Term, defs: Seq[Definition[Inline]]) extends DefinitionList.Item[Inline]
  }
}

sealed abstract class OrderedList extends List {
  type Item <: OrderedList.Item[_]
  def counter: OrderedList.Counter
}

object OrderedList {
  case class Loose(counter: Counter, items: Seq[OrderedList.Loose.Item]) extends OrderedList {
    type Item = OrderedList.Loose.Item
    def accept[A](visitor: Block.Visitor[A]): A = visitor.visit(this)
  }

  object Loose {
    case class Item(content: Seq[Block]) extends OrderedList.Item[Block]
  }

  case class Tight(counter: Counter, items: Seq[OrderedList.Tight.Item]) extends OrderedList {
    type Item = OrderedList.Tight.Item
    def accept[A](visitor: Block.Visitor[A]): A = visitor.visit(this)
  }

  object Tight {
    case class Item(content: Seq[Inline]) extends OrderedList.Item[Inline]
  }

  sealed abstract class Item[+A <: Markdown] extends Composite[A]

  /** Counter information for an ordered list.
    *
    * @param start the initial value for the list counter, if it could be determined.
    * @param name the name of the list counter, if it was specified.
    */
  case class Counter(
    numeralStyle: NumeralStyle,
    separatorStyle: SeparatorStyle,
    start: Option[Int],
    name: Option[String]
  )

  sealed abstract class NumeralStyle {
    def encode(i: Int): Option[String]
    def decode(s: CharSequence): Option[Int]
  }

  object NumeralStyle {
    case object Arabic extends NumeralStyle {
      def encode(i: Int): Option[String]       = NumeralSystem.Arabic.encode(i)
      def decode(s: CharSequence): Option[Int] = NumeralSystem.Arabic.decode(s)
    }

    case object LowerAlpha extends NumeralStyle {
      def encode(i: Int): Option[String]       = NumeralSystem.Alpha.encode(i)
      def decode(s: CharSequence): Option[Int] = NumeralSystem.Alpha.decode(s)
    }

    case object UpperAlpha extends NumeralStyle {
      def encode(i: Int): Option[String]       = NumeralSystem.Alpha.encode(i).map(_.toUpperCase)
      def decode(s: CharSequence): Option[Int] = NumeralSystem.Alpha.decode(s)
    }

    case object LowerRoman extends NumeralStyle {
      def encode(i: Int): Option[String]       = NumeralSystem.Roman.encode(i)
      def decode(s: CharSequence): Option[Int] = NumeralSystem.Roman.decode(s)
    }

    case object UpperRoman extends NumeralStyle {
      def encode(i: Int): Option[String]       = NumeralSystem.Roman.encode(i).map(_.toUpperCase)
      def decode(s: CharSequence): Option[Int] = NumeralSystem.Roman.decode(s)
    }
  }

  sealed abstract class SeparatorStyle
  object SeparatorStyle {
    /** [[smd.dom.OrderedList.SeparatorStyle]] for a counter followed by a single dot. */
    case object TrailingDot extends SeparatorStyle
    /** [[smd.dom.OrderedList.SeparatorStyle]] for a counter followed by a single right parenthesis. */
    case object TrailingParenthesis extends SeparatorStyle
    /** [[smd.dom.OrderedList.SeparatorStyle]] for a counter enclosed by round parentheses. */
    case object EnclosingParentheses extends SeparatorStyle
  }
}

sealed abstract class UnorderedList extends List {
  type Item <: UnorderedList.Item[_]
}

object UnorderedList {
  case class Loose(items: Seq[UnorderedList.Loose.Item]) extends UnorderedList {
    type Item = UnorderedList.Loose.Item
    def accept[A](visitor: Block.Visitor[A]): A = visitor.visit(this)
  }

  object Loose {
    case class Item(content: Seq[Block]) extends UnorderedList.Item[Block]
  }

  case class Tight(items: Seq[UnorderedList.Tight.Item]) extends UnorderedList {
    type Item = UnorderedList.Tight.Item
    def accept[A](visitor: Block.Visitor[A]): A = visitor.visit(this)
  }

  object Tight {
    case class Item(content: Seq[Inline]) extends UnorderedList.Item[Inline]
  }

  sealed abstract class Item[+A <: Markdown] extends Composite[A]
}

//endregion

//endregion

//region Inline

sealed abstract class Atomic extends Inline with Visitable[Atomic.Visitor] {
}

object Atomic {
  trait Visitor[+A] {
    def visit(node: Attributes): A
    def visit(node: AutoLink): A
    def visit(node: Code): A
    def visit(node: InlineExpression): A
    def visit(node: LineBreak): A
    def visit(node: Space): A
    def visit(node: Symbol): A
    def visit(node: Text): A
    def visit(node: Entity): A
  }
}

sealed abstract class Span extends Inline with Composite[Inline] with Visitable[Span.Visitor] {
}

object Span {
  trait Visitor[+A] {
    def visit(node: Emphasis): A
    def visit(node: Link): A
    def visit(node: Quoted): A
    def visit(node: Strong): A
    def visit(node: Subscript): A
    def visit(node: Superscript): A
  }
}

//region Atomic

case class Attributes(attrs: Seq[Attribute]) extends Atomic {
  def accept[A](visitor: Atomic.Visitor[A]): A = visitor.visit(this)
}

case class AutoLink(uri: String) extends Atomic {
  def accept[A](visitor: Atomic.Visitor[A]): A = visitor.visit(this)
}

case class Code(value: String) extends Atomic {
  def accept[A](visitor: Atomic.Visitor[A]): A = visitor.visit(this)
}

case class InlineExpression(expr: Expression) extends Atomic {
  def accept[A](visitor: Atomic.Visitor[A]): A = visitor.visit(this)
}

case class LineBreak() extends Atomic {
  def accept[A](visitor: Atomic.Visitor[A]): A = visitor.visit(this)
}

case class Space() extends Atomic {
  def accept[A](visitor: Atomic.Visitor[A]): A = visitor.visit(this)
}

case class Symbol(value: String) extends Atomic {
  def accept[A](visitor: Atomic.Visitor[A]): A = visitor.visit(this)
}

case class Text(value: String) extends Atomic {
  def accept[A](visitor: Atomic.Visitor[A]): A = visitor.visit(this)
}

case class Entity(codePoints: Seq[Int]) extends Atomic {
  def accept[A](visitor: Atomic.Visitor[A]): A = visitor.visit(this)
}

//endregion

//region Span

case class Emphasis(content: Seq[Inline]) extends Span {
  def accept[A](visitor: Span.Visitor[A]): A = visitor.visit(this)
}

case class Link(content: Seq[Inline], ref: Option[ReferenceId], args: Seq[Argument]) extends Span {
  def accept[A](visitor: Span.Visitor[A]): A = visitor.visit(this)
}

case class Quoted(content: Seq[Inline], kind: Quoted.QuoteKind) extends Span {
  def accept[A](visitor: Span.Visitor[A]): A = visitor.visit(this)
}

object Quoted {
  sealed abstract class QuoteKind
  object QuoteKind {
    case object Double extends QuoteKind
    case object Single extends QuoteKind
  }
}

case class Strong(content: Seq[Inline]) extends Span {
  def accept[A](visitor: Span.Visitor[A]): A = visitor.visit(this)
}

case class Subscript(content: Seq[Inline]) extends Span {
  def accept[A](visitor: Span.Visitor[A]): A = visitor.visit(this)
}

case class Superscript(content: Seq[Inline]) extends Span {
  def accept[A](visitor: Span.Visitor[A]): A = visitor.visit(this)
}

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

case class ArrayLiteral(elems: Seq[Option[Expression]]) extends Expression {
  def accept[A](visitor: Expression.Visitor[A]): A = visitor.visit(this)
}

case class BooleanLiteral(value: Boolean) extends Expression {
  def accept[A](visitor: Expression.Visitor[A]): A = visitor.visit(this)
}

case class DocumentLiteral(doc: Document) extends Expression {
  def accept[A](visitor: Expression.Visitor[A]): A = visitor.visit(this)
}

case class NullLiteral() extends Expression {
  def accept[A](visitor: Expression.Visitor[A]): A = visitor.visit(this)
}

case class NumericLiteral(value: Double) extends Expression {
  def accept[A](visitor: Expression.Visitor[A]): A = visitor.visit(this)
}

case class ObjectLiteral(props: Seq[Attribute]) extends Expression {
  def accept[A](visitor: Expression.Visitor[A]): A = visitor.visit(this)
}

object ObjectLiteral {
  def apply(): ObjectLiteral = apply(Seq())
  def apply(p0: Attribute, ps: Attribute*): ObjectLiteral = apply(p0 +: ps)
}

/** A property of an [[smd.dom.ObjectLiteral]] or [[smd.dom.Attributes]]. */
sealed case class Attribute(name: String, value: Expression)

object Attribute {
  implicit def tuple2Property(tup: (String, Expression)): Attribute = Attribute(tup._1, tup._2)
}

sealed abstract class StringLikeLiteral extends Expression {
  def value: String
}

case class StringLiteral(value: String) extends StringLikeLiteral {
  def accept[A](visitor: Expression.Visitor[A]): A = visitor.visit(this)
}

case class VerbatimLiteral(value: String) extends StringLikeLiteral {
  def accept[A](visitor: Expression.Visitor[A]): A = visitor.visit(this)
}

case class IriLiteral(value: String) extends StringLikeLiteral {
  def accept[A](visitor: Expression.Visitor[A]): A = visitor.visit(this)
}

//endregion

case class Addition(lhs: Expression, rhs: Expression) extends Expression with Binary {
  def accept[A](visitor: Expression.Visitor[A]): A = visitor.visit(this)
}

case class Application(body: Expression, args: Seq[Argument]) extends Expression {
  def accept[A](visitor: Expression.Visitor[A]): A = visitor.visit(this)
}

case class BitwiseAnd(lhs: Expression, rhs: Expression) extends Expression with Binary {
  def accept[A](visitor: Expression.Visitor[A]): A = visitor.visit(this)
}

case class BitwiseNot(expr: Expression) extends Expression with Unary {
  def accept[A](visitor: Expression.Visitor[A]): A = visitor.visit(this)
}

case class BitwiseOr(lhs: Expression, rhs: Expression) extends Expression with Binary {
  def accept[A](visitor: Expression.Visitor[A]): A = visitor.visit(this)
}

case class BitwiseXOr(lhs: Expression, rhs: Expression) extends Expression with Binary {
  def accept[A](visitor: Expression.Visitor[A]): A = visitor.visit(this)
}

case class Conditional(cond: Expression, thenExpr: Expression, elseExpr: Option[Expression] = None) extends Expression {
  def accept[A](visitor: Expression.Visitor[A]): A = visitor.visit(this)
}

case class Division(lhs: Expression, rhs: Expression) extends Expression with Binary {
  def accept[A](visitor: Expression.Visitor[A]): A = visitor.visit(this)
}

case class Equals(lhs: Expression, rhs: Expression) extends Expression with Binary {
  def accept[A](visitor: Expression.Visitor[A]): A = visitor.visit(this)
}

case class GreaterThan(lhs: Expression, rhs: Expression) extends Expression with Binary {
  def accept[A](visitor: Expression.Visitor[A]): A = visitor.visit(this)
}

case class GreaterThanOrEqualTo(lhs: Expression, rhs: Expression) extends Expression with Binary {
  def accept[A](visitor: Expression.Visitor[A]): A = visitor.visit(this)
}

case class Identifier(name: String) extends Expression {
  def accept[A](visitor: Expression.Visitor[A]): A = visitor.visit(this)
}

case class LeftShift(lhs: Expression, rhs: Expression) extends Expression with Binary {
  def accept[A](visitor: Expression.Visitor[A]): A = visitor.visit(this)
}

case class LessThan(lhs: Expression, rhs: Expression) extends Expression with Binary {
  def accept[A](visitor: Expression.Visitor[A]): A = visitor.visit(this)
}

case class LessThanOrEqualTo(lhs: Expression, rhs: Expression) extends Expression with Binary {
  def accept[A](visitor: Expression.Visitor[A]): A = visitor.visit(this)
}

case class LogicalAnd(lhs: Expression, rhs: Expression) extends Expression with Binary {
  def accept[A](visitor: Expression.Visitor[A]): A = visitor.visit(this)
}

case class LogicalNot(expr: Expression) extends Expression with Unary {
  def accept[A](visitor: Expression.Visitor[A]): A = visitor.visit(this)
}

case class LogicalOr(lhs: Expression, rhs: Expression) extends Expression with Binary {
  def accept[A](visitor: Expression.Visitor[A]): A = visitor.visit(this)
}

case class Member(body: Expression, name: String) extends Expression {
  def accept[A](visitor: Expression.Visitor[A]): A = visitor.visit(this)
}

case class Modulo(lhs: Expression, rhs: Expression) extends Expression with Binary {
  def accept[A](visitor: Expression.Visitor[A]): A = visitor.visit(this)
}

case class Multiplication(lhs: Expression, rhs: Expression) extends Expression with Binary {
  def accept[A](visitor: Expression.Visitor[A]): A = visitor.visit(this)
}

case class Negative(expr: Expression) extends Expression with Unary {
  def accept[A](visitor: Expression.Visitor[A]): A = visitor.visit(this)
}

case class NotEquals(lhs: Expression, rhs: Expression) extends Expression with Binary {
  def accept[A](visitor: Expression.Visitor[A]): A = visitor.visit(this)
}

case class Positive(expr: Expression) extends Expression with Unary {
  def accept[A](visitor: Expression.Visitor[A]): A = visitor.visit(this)
}

case class RightShift(lhs: Expression, rhs: Expression) extends Expression with Binary {
  def accept[A](visitor: Expression.Visitor[A]): A = visitor.visit(this)
}

case class StrictEquals(lhs: Expression, rhs: Expression) extends Expression with Binary {
  def accept[A](visitor: Expression.Visitor[A]): A = visitor.visit(this)
}

case class StrictNotEquals(lhs: Expression, rhs: Expression) extends Expression with Binary {
  def accept[A](visitor: Expression.Visitor[A]): A = visitor.visit(this)
}

case class Subtraction(lhs: Expression, rhs: Expression) extends Expression with Binary {
  def accept[A](visitor: Expression.Visitor[A]): A = visitor.visit(this)
}

case class UnsignedRightShift(lhs: Expression, rhs: Expression) extends Expression with Binary {
  def accept[A](visitor: Expression.Visitor[A]): A = visitor.visit(this)
}

//region Misc

case class Argument(name: Option[String], value: Expression) {
  def isNamed: Boolean = name.isDefined
}

object Argument {
  implicit def fromTuple(t: (String, Expression)): Argument = t match { case (n, v) => named(n, v) }
  implicit def fromExpression(e: Expression): Argument = unnamed(e)
  def named(n: String, v: Expression): Argument = Argument(Some(n), v)
  def unnamed(v: Expression): Argument = Argument(None, v)
}

//endregion

//endregion
