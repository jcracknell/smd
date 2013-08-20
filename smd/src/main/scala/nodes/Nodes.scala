package smd
package nodes

import expressions.Expression

sealed abstract class Node
sealed abstract class BlockNode extends Node
sealed abstract class InlineNode extends Node

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

case class AutoLinkNode(uri: String, args: Seq[Expression]) extends InlineNode
case class BlockquoteNode(children: Seq[BlockNode]) extends BlockNode
case class CodeNode(value: String) extends InlineNode
case class DefinitionListDefinitionNode(children: Seq[Node]) extends Node
case class DefinitionListItemNode(term: DefinitionListTermNode, defs: Seq[DefinitionListDefinitionNode]) extends Node
case class DefinitionListNode(children: DefinitionListItemNode) extends BlockNode
case class DefinitionListTermNode(children: Seq[Node]) extends Node
case class DocumentNode(children: Seq[BlockNode]) extends Node
case class EmphasisNode(children: Seq[InlineNode]) extends InlineNode
case class EntityNode(value: Int) extends InlineNode
case class ExpressionBlockNode(expr: Expression) extends BlockNode
case class HeadingNode(children: Seq[InlineNode], level: Int) extends BlockNode
case class InlineExpressionNode(expr: Expression) extends InlineNode
case class LineBreakNode() extends InlineNode
case class LinkNode(children: Seq[InlineNode], ref: Option[ReferenceId], args: Seq[Expression]) extends Node
case class OrderedListItemNode(children: Seq[Node]) extends Node
case class OrderedListNode(children: Seq[OrderedListItemNode], style: OrderedListCounterStyle) extends Node
case class ParagraphNode(children: Seq[BlockNode]) extends InlineNode
case class QuotedNode(children: Seq[InlineNode], kind: QuoteKind) extends Node
case class ReferenceId(value: String)
case class ReferenceNode(ref: ReferenceId, args: Seq[Expression]) extends BlockNode
case class SpaceNode() extends Node
case class StrongNode(children: Seq[InlineNode]) extends Node
case class SymbolNode(value: String) extends Node
case class TextNode(value: String) extends Node
case class UnorderedListItemNode(children: Seq[Node]) extends Node
case class UnorderedListNode(children: Seq[UnorderedListItemNode]) extends BlockNode

