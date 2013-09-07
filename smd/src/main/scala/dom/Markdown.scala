package smd
package dom

sealed abstract class MarkDown

object MarkDown {
  sealed abstract class Block extends MarkDown
  sealed abstract class Inline extends MarkDown

  sealed trait OrderedListCounterStyle
  object OrderedListCounterStyle {
    case object Arabic extends OrderedListCounterStyle
    case object LowerAlpha extends OrderedListCounterStyle
    case object UpperAlpha extends OrderedListCounterStyle
    case object LowerRoman extends OrderedListCounterStyle
    case object UpperRoman extends OrderedListCounterStyle
  }

  sealed trait QuoteKind
  object QuoteKind {
    case object DoubleQuote extends QuoteKind
    case object SingleQuote extends QuoteKind
  }

  case class AutoLink(uri: String, args: Seq[Expression]) extends Inline
  case class Blockquote(children: Seq[Block]) extends Block
  case class Code(value: String) extends Inline
  case class DefinitionListDefinition(children: Seq[MarkDown]) extends MarkDown
  case class DefinitionListItem(term: DefinitionListTerm, defs: Seq[DefinitionListDefinition]) extends MarkDown
  case class DefinitionList(children: DefinitionListItem) extends Block
  case class DefinitionListTerm(children: Seq[MarkDown]) extends MarkDown
  case class Document(children: Seq[Block]) extends MarkDown
  case class Emphasis(children: Seq[Inline]) extends Inline
  case class Entity(value: Int) extends Inline
  case class ExpressionBlock(expr: Expression) extends Block
  case class Heading(children: Seq[Inline], level: Int) extends Block
  case class InlineExpression(expr: Expression) extends Inline
  case class LineBreak() extends Inline
  case class Link(children: Seq[Inline], ref: Option[ReferenceId], args: Seq[Expression]) extends Inline
  case class OrderedListItem(children: Seq[MarkDown]) extends MarkDown
  case class OrderedList(children: Seq[OrderedListItem], style: OrderedListCounterStyle) extends MarkDown
  case class Paragraph(children: Seq[Block]) extends Inline
  case class Quoted(children: Seq[Inline], kind: QuoteKind) extends Inline
  case class ReferenceId(value: String)
  case class Reference(ref: ReferenceId, args: Seq[Expression]) extends Block
  case class Space() extends Inline
  case class Strong(children: Seq[Inline]) extends Inline
  case class Symbol(value: String) extends Inline
  case class Text(value: String) extends Inline
  case class UnorderedListItem(children: Seq[MarkDown]) extends MarkDown
  case class UnorderedList(children: Seq[UnorderedListItem]) extends Block
}
