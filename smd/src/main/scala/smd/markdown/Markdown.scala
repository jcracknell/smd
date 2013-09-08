package smd
package markdown

abstract class Markdown

trait Composite[+A] {
  def children: Seq[A]
}

case class Document(children: Seq[Block]) extends Markdown with Composite[Block]

sealed abstract class Block extends Markdown

case class Blockquote(children: Seq[Block]) extends Block with Composite[Block]
case class ExpressionBlock(expr: expression.Expression) extends Block
case class Heading(children: Seq[Inline], level: Int) extends Block with Composite[Inline]
case class Paragraph(children: Seq[Inline]) extends Block with Composite[Inline]
case class Reference(ref: ReferenceId, args: Seq[expression.Expression]) extends Block

sealed abstract class List extends Block {
  type Item
  def items: Seq[Item]
}

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

sealed abstract class UnorderedList {
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

sealed abstract class Inline extends Markdown

sealed abstract class Span extends Inline with Composite[Inline]

case class Emphasis(children: Seq[Inline]) extends Span
case class Link(children: Seq[Inline], ref: Option[ReferenceId], args: Seq[expression.Expression]) extends Span
case class Quoted(children: Seq[Inline], kind: Quoted.QuoteKind) extends Span

object Quoted {
  sealed abstract class QuoteKind
  object QuoteKind {
    case object Double extends QuoteKind
    case object Single extends QuoteKind
  }
}

case class Strong(children: Seq[Inline]) extends Span

sealed abstract class Atomic extends Inline

case class AutoLink(uri: String, args: Seq[expression.Expression]) extends Atomic
case class Code(value: String) extends Atomic
case class InlineExpression(expr: expression.Expression) extends Atomic
case class LineBreak() extends Atomic
case class Space() extends Atomic
case class Symbol(value: String) extends Atomic
case class Text(value: String) extends Atomic

sealed abstract class Entity extends Atomic

case class NamedEntity(name: String) extends Entity
case class NumericEntity(value: Int) extends Entity


case class ReferenceId(value: String)
