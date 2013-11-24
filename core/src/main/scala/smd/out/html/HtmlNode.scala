package smd.out.html

import smd.util.HtmlUtils

/** Class forming the basis for a simple DOM for generating HTML documents in a convenient manner. */
sealed abstract class HtmlNode {
  def accept[A](visitor: HtmlNode.Visitor[A]): A
}

object HtmlNode {
  trait Element {
    def tag: String
    def attributes: Seq[HtmlAttribute]
  }

  trait CompositeElement extends Element {
    def children: Seq[HtmlNode]
  }

  trait Visitor[+A] {
    def visit(node: Comment): A
    def visit(node: Text): A
    def visit(node: Empty): A
    def visit(node: Block): A
    def visit(node: Inline): A
    def visit(node: Pre): A
  }

  case class Comment(text: HtmlString) extends HtmlNode {
    def accept[A](visitor: Visitor[A]): A = visitor.visit(this)
  }

  case class Text(text: HtmlString) extends HtmlNode {
    def accept[A](visitor: Visitor[A]): A = visitor.visit(this)
  }

  case class Empty(tag: String, attributes: Seq[HtmlAttribute]) extends HtmlNode with Element {
    def accept[A](visitor: Visitor[A]): A = visitor.visit(this)
  }

  case class Block(tag: String, attributes: Seq[HtmlAttribute], children: Seq[HtmlNode]) extends HtmlNode with CompositeElement {
    def accept[A](visitor: Visitor[A]): A = visitor.visit(this)
  }

  case class Inline(tag: String, attributes: Seq[HtmlAttribute], children: Seq[HtmlNode]) extends HtmlNode with CompositeElement {
    def accept[A](visitor: Visitor[A]): A = visitor.visit(this)
  }

  case class Pre(tag: String, attributes: Seq[HtmlAttribute], children: Seq[HtmlNode]) extends HtmlNode with CompositeElement {
    def accept[A](visitor: Visitor[A]): A = visitor.visit(this)
  }
}

/** Class encapsulating an HTML-encoded string value. */
case class HtmlString(value: String)

object HtmlString {
  import scala.language.implicitConversions

  implicit def encode(value: String): HtmlString = new HtmlString(HtmlUtils.htmlEncode(value))
  def raw(value: String): HtmlString = new HtmlString(value)
}

/** Class encapsulating an optionally defined HTML attribute. */
sealed abstract class HtmlAttribute {
  /** The name of the attribute. */
  def name: String
}

object HtmlAttribute {
  case class Boolean(name: String) extends HtmlAttribute
  case class Value(name: String, value: HtmlString) extends HtmlAttribute
}
