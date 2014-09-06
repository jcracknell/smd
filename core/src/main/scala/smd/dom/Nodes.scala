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

trait MarkdownLike {
  def sourceRange: SourceRange
}

sealed abstract class Markdown extends Node with Visitable[Markdown.Visitor] with MarkdownLike

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
    def visit(node: BlockLiteral): A
    def visit(node: BooleanLiteral): A
    def visit(node: NullLiteral): A
    def visit(node: NumericLiteral): A
    def visit(node: ObjectLiteral): A
    def visit(node: StringLiteral): A
    def visit(node: VerbatimLiteral): A
    def visit(node: InlineLiteral): A
    def visit(node: IriLiteral): A
    def visit(node: Addition): A
    def visit(node: Application): A
    def visit(node: Conditional): A
    def visit(node: Division): A
    def visit(node: Equals): A
    def visit(node: Exponentiation): A
    def visit(node: GreaterThan): A
    def visit(node: GreaterThanOrEqualTo): A
    def visit(node: Identifier): A
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
    def visit(node: StrictEquals): A
    def visit(node: StrictNotEquals): A
    def visit(node: Subtraction): A
  }
}

//region Markdown

sealed abstract class Block extends Markdown with Visitable[Block.Visitor]

object Block {
  trait Visitor[+A] {
    def visit(node: Blockquote): A
    def visit(node: ExpressionBlock): A
    def visit(node: Heading): A
    def visit(node: LooseDefinitionList): A
    def visit(node: LooseOrderedList): A
    def visit(node: LooseUnorderedList): A
    def visit(node: Paragraph): A
    def visit(node: Reference): A
    def visit(node: Table): A
    def visit(node: TightDefinitionList): A
    def visit(node: TightOrderedList): A
    def visit(node: TightUnorderedList): A
  }

  trait VisitableImpl[A] extends Visitable[Block.Visitor] {
    def accept[B](visitor: Block.Visitor[B]): B
  }
}

sealed abstract class Inline extends Markdown with Visitable[Inline.Visitor]

object Inline {
  trait Visitor[+A] extends Atomic.Visitor[A] with Span.Visitor[A]
}

//region Block

case class Blockquote(sourceRange: SourceRange, content: Seq[Block]) extends Block with Composite[Block] {
  def accept[A](visitor: Block.Visitor[A]): A = visitor.visit(this)
}

case class ExpressionBlock(sourceRange: SourceRange, expr: Expression) extends Block {
  def accept[A](visitor: Block.Visitor[A]): A = visitor.visit(this)
}

case class Heading(sourceRange: SourceRange, level: Int, content: Seq[Inline]) extends Block with Composite[Inline] {
  def accept[A](visitor: Block.Visitor[A]): A = visitor.visit(this)
}

case class Paragraph(sourceRange: SourceRange, content: Seq[Inline]) extends Block with Composite[Inline] {
  def accept[A](visitor: Block.Visitor[A]): A = visitor.visit(this)
}

case class Reference(sourceRange: SourceRange, ref: ReferenceId, args: Seq[Argument]) extends Block {
  def accept[A](visitor: Block.Visitor[A]): A = visitor.visit(this)
}

case class Table(sourceRange: SourceRange, head: Seq[Table.Row], body: Seq[Table.Row]) extends Block {
  def accept[A](visitor: Block.Visitor[A]): A = visitor.visit(this)
}

object Table {
  case class Row(cells: Cell*)
  case class Cell(sourceRange: SourceRange, alignment: CellAlignment, span: Int, content: Seq[Inline]) extends Composite[Inline] with MarkdownLike

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

trait LooseList { self: List =>
  type Item <: LooseList.Item
}

object LooseList {
  trait Item extends Composite[Block] with MarkdownLike
}

trait TightList { self: List =>
  type Item <: TightList.Item
}

object TightList {
  trait Item extends Composite[Inline] with MarkdownLike {
    def sublists: Seq[List]
  }
}

//region Lists

sealed abstract class DefinitionList extends List {
  type Item <: DefinitionList.Item
}

object DefinitionList {
  case class Term(sourceRange: SourceRange, content: Seq[Inline]) extends Composite[Inline]
  
  sealed abstract class Definition extends MarkdownLike

  sealed abstract class Item extends MarkdownLike {
    type Definition 
    def term: Term
    def defs: Seq[Definition]
  }
}

case class LooseDefinitionList(sourceRange: SourceRange, items: Seq[LooseDefinitionList.Item]) extends DefinitionList {
  type Item = LooseDefinitionList.Item
  def accept[A](visitor: Block.Visitor[A]): A = visitor.visit(this)
}

object LooseDefinitionList {
  case class Item(sourceRange: SourceRange, term: DefinitionList.Term, defs: Seq[LooseDefinitionList.Definition]) extends DefinitionList.Item {
    type Definition = LooseDefinitionList.Definition
  }

  case class Definition(sourceRange: SourceRange, content: Seq[Block]) extends DefinitionList.Definition with LooseList.Item
}

case class TightDefinitionList(sourceRange: SourceRange, items: Seq[TightDefinitionList.Item]) extends DefinitionList {
  type Item = TightDefinitionList.Item
  def accept[A](visitor: Block.Visitor[A]): A = visitor.visit(this)
}

object TightDefinitionList {
  case class Item(sourceRange: SourceRange, term: DefinitionList.Term, defs: Seq[TightDefinitionList.Definition]) extends DefinitionList.Item {
    type Definition = TightDefinitionList.Definition
  }

  case class Definition(sourceRange: SourceRange, content: Seq[Inline], sublists: Seq[List] = Nil) extends DefinitionList.Definition with TightList.Item
}

sealed abstract class OrderedList extends List {
  def counter: OrderedList.Counter
}

object OrderedList {
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

case class LooseOrderedList(sourceRange: SourceRange, counter: OrderedList.Counter, items: Seq[LooseOrderedList.Item]) extends OrderedList with LooseList {
  type Item = LooseOrderedList.Item
  def accept[A](visitor: Block.Visitor[A]): A = visitor.visit(this)
}

object LooseOrderedList {
  case class Item(sourceRange: SourceRange, content: Seq[Block]) extends LooseList.Item
}

case class TightOrderedList(sourceRange: SourceRange, counter: OrderedList.Counter, items: Seq[TightOrderedList.Item]) extends OrderedList with TightList {
  type Item = TightOrderedList.Item
  def accept[A](visitor: Block.Visitor[A]): A = visitor.visit(this)
}

object TightOrderedList {
  case class Item(sourceRange: SourceRange, content: Seq[Inline], sublists: Seq[List] = Nil) extends TightList.Item
}

sealed abstract class UnorderedList extends List

case class LooseUnorderedList(sourceRange: SourceRange, items: Seq[LooseUnorderedList.Item]) extends UnorderedList with LooseList {
  type Item = LooseUnorderedList.Item
  def accept[A](visitor: Block.Visitor[A]): A = visitor.visit(this)
}

object LooseUnorderedList {
  case class Item(sourceRange: SourceRange, content: Seq[Block]) extends LooseList.Item
}

case class TightUnorderedList(sourceRange: SourceRange, items: Seq[TightUnorderedList.Item]) extends UnorderedList with TightList {
  type Item = TightUnorderedList.Item
  def accept[A](visitor: Block.Visitor[A]): A = visitor.visit(this)
}

object TightUnorderedList {
  case class Item(sourceRange: SourceRange, content: Seq[Inline], sublists: Seq[List] = Nil) extends TightList.Item
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

case class Attributes(sourceRange: SourceRange, attrs: Seq[Attributes.Attribute]) extends Atomic {
  def accept[A](visitor: Atomic.Visitor[A]): A = visitor.visit(this)
}

object Attributes {
  case class Attribute(name: String, value: Expression)

  object Attribute {
    implicit def tuple2ToAttribute(tup: (String, Expression)): Attribute = Attribute(tup._1, tup._2)
  }
}

case class AutoLink(sourceRange: SourceRange, uri: String) extends Atomic {
  def accept[A](visitor: Atomic.Visitor[A]): A = visitor.visit(this)
}

case class Code(sourceRange: SourceRange, value: String) extends Atomic {
  def accept[A](visitor: Atomic.Visitor[A]): A = visitor.visit(this)
}

case class InlineExpression(sourceRange: SourceRange, expr: Expression) extends Atomic {
  def accept[A](visitor: Atomic.Visitor[A]): A = visitor.visit(this)
}

case class LineBreak(sourceRange: SourceRange) extends Atomic {
  def accept[A](visitor: Atomic.Visitor[A]): A = visitor.visit(this)
}

case class Space(sourceRange: SourceRange) extends Atomic {
  def accept[A](visitor: Atomic.Visitor[A]): A = visitor.visit(this)
}

case class Symbol(sourceRange: SourceRange, value: String) extends Atomic {
  def accept[A](visitor: Atomic.Visitor[A]): A = visitor.visit(this)
}

case class Text(sourceRange: SourceRange, value: String) extends Atomic {
  def accept[A](visitor: Atomic.Visitor[A]): A = visitor.visit(this)
}

case class Entity(sourceRange: SourceRange, codePoints: Seq[Int]) extends Atomic {
  def accept[A](visitor: Atomic.Visitor[A]): A = visitor.visit(this)
}

//endregion

//region Span

case class Emphasis(sourceRange: SourceRange, content: Seq[Inline]) extends Span {
  def accept[A](visitor: Span.Visitor[A]): A = visitor.visit(this)
}

case class Link(sourceRange: SourceRange, content: Seq[Inline], ref: Option[ReferenceId], args: Seq[Argument]) extends Span {
  def accept[A](visitor: Span.Visitor[A]): A = visitor.visit(this)
}

case class Quoted(sourceRange: SourceRange, content: Seq[Inline], kind: Quoted.QuoteKind) extends Span {
  def accept[A](visitor: Span.Visitor[A]): A = visitor.visit(this)
}

object Quoted {
  sealed abstract class QuoteKind
  object QuoteKind {
    case object Double extends QuoteKind
    case object Single extends QuoteKind
  }
}

case class Strong(sourceRange: SourceRange, content: Seq[Inline]) extends Span {
  def accept[A](visitor: Span.Visitor[A]): A = visitor.visit(this)
}

case class Subscript(sourceRange: SourceRange, content: Seq[Inline]) extends Span {
  def accept[A](visitor: Span.Visitor[A]): A = visitor.visit(this)
}

case class Superscript(sourceRange: SourceRange, content: Seq[Inline]) extends Span {
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

case class BlockLiteral(blocks: Seq[Block]) extends Expression {
  def accept[A](visitor: Expression.Visitor[A]): A = visitor.visit(this)
}

case class BooleanLiteral(value: Boolean) extends Expression {
  def accept[A](visitor: Expression.Visitor[A]): A = visitor.visit(this)
}

case class InlineLiteral(inlines: Seq[Inline]) extends Expression {
  def accept[A](visitor: Expression.Visitor[A]): A = visitor.visit(this)
}

case class NullLiteral() extends Expression {
  def accept[A](visitor: Expression.Visitor[A]): A = visitor.visit(this)
}

case class NumericLiteral(value: Double) extends Expression {
  def accept[A](visitor: Expression.Visitor[A]): A = visitor.visit(this)
}

case class ObjectLiteral(props: Seq[ObjectLiteral.Property]) extends Expression {
  def accept[A](visitor: Expression.Visitor[A]): A = visitor.visit(this)
}

object ObjectLiteral {
  case class Property(name: String, value: Expression)

  object Property {
    implicit def tuple2ToProperty(tup: (String, Expression)): Property = Property(tup._1, tup._2)
  }
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

case class Conditional(cond: Expression, thenExpr: Expression, elseExpr: Option[Expression] = None) extends Expression {
  def accept[A](visitor: Expression.Visitor[A]): A = visitor.visit(this)
}

case class Division(lhs: Expression, rhs: Expression) extends Expression with Binary {
  def accept[A](visitor: Expression.Visitor[A]): A = visitor.visit(this)
}

case class Equals(lhs: Expression, rhs: Expression) extends Expression with Binary {
  def accept[A](visitor: Expression.Visitor[A]): A = visitor.visit(this)
}

case class Exponentiation(lhs: Expression, rhs: Expression) extends Expression with Binary {
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

case class StrictEquals(lhs: Expression, rhs: Expression) extends Expression with Binary {
  def accept[A](visitor: Expression.Visitor[A]): A = visitor.visit(this)
}

case class StrictNotEquals(lhs: Expression, rhs: Expression) extends Expression with Binary {
  def accept[A](visitor: Expression.Visitor[A]): A = visitor.visit(this)
}

case class Subtraction(lhs: Expression, rhs: Expression) extends Expression with Binary {
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
