package smd
package out
package html

/** Trait encapsulating a conversion to a sequence of [[smd.out.html.HtmlNode]], allowing nodes and
  * sequences of nodes to be handled in a uniform manner. */
trait HtmlNodeable {
  def toNodes: Seq[HtmlNode]
}

object HtmlNodeable {
  import scala.language.implicitConversions

  def apply(mkNodes: => Seq[HtmlNode]): HtmlNodeable = new HtmlNodeable {
    private lazy val nodes = mkNodes
    def toNodes: Seq[HtmlNode] = nodes
  }

  implicit def fromNode(node: HtmlNode): HtmlNodeable = HtmlNodeable {
    Seq(node)
  }

  implicit def fromString(text: String): HtmlNodeable = HtmlNodeable {
    Seq(HtmlNode.Text(HtmlString.encode(text)))
  }

  implicit def fromHtmlString(text: HtmlString): HtmlNodeable = HtmlNodeable {
    Seq(HtmlNode.Text(text))
  }

  implicit def fromNodeableSeq[A <% HtmlNodeable](nodeables: Seq[A]): HtmlNodeable = HtmlNodeable {
    nodeables flatMap { _.toNodes }
  }
}

trait HtmlAttributable {
  def toAttributes: Seq[HtmlAttribute]
}

object HtmlAttributable {
  import scala.language.implicitConversions

  def apply(mkAttributes: => Seq[HtmlAttribute]): HtmlAttributable = new HtmlAttributable {
    private lazy val attributes = mkAttributes
    def toAttributes: Seq[HtmlAttribute] = attributes
  }

  implicit def valueFromAttribute(attr: HtmlAttribute) = HtmlAttributable { Seq(attr) }

  implicit def valueFromSymbolAndHtmlString(tup: (Symbol, HtmlString)) = HtmlAttributable {
    tup match { case (s, value) => Seq(HtmlAttribute.Value(s.name, value)) }
  }

  implicit def valueFromStringAndHtmlString(tup: (String, HtmlString)) = HtmlAttributable {
    tup match { case (name, value) => Seq(HtmlAttribute.Value(name, value)) }
  }

  implicit def valueFromSymbolAndAny[A](tup: (Symbol, A)) = HtmlAttributable {
    tup match { case (s, a) => Seq(HtmlAttribute.Value(s.name, HtmlString.encode(a.toString))) }
  }

  implicit def valueFromStringAndAny[A](tup: (String, A)) = HtmlAttributable {
    tup match { case (name, a) => Seq(HtmlAttribute.Value(name, HtmlString.encode(a.toString))) }
  }

  implicit def booleanFromSymbol(s: Symbol) = HtmlAttributable { Seq(HtmlAttribute.Boolean(s.name)) }

  implicit def booleanFromString(name: String) = HtmlAttributable { Seq(HtmlAttribute.Boolean(name)) }

  implicit def valueFromAttributableOption[A <% HtmlAttributable](opt: Option[A]): HtmlAttributable = HtmlAttributable {
    opt.map(_.toAttributes).getOrElse(Nil)
  }

  implicit def valueFromAttributableSeq[A <% HtmlAttributable](seq: Seq[A]): HtmlAttributable = HtmlAttributable {
    seq.flatMap(_.toAttributes)
  }
}

trait HtmlFu {
  protected def tag: String

  def block(attrs: HtmlAttributable*)(children: HtmlNodeable*) =
    HtmlNode.Block(tag, attrs.flatMap(_.toAttributes), children.flatMap(_.toNodes))

  def inline(attrs: HtmlAttributable*)(children: HtmlNodeable*) =
    HtmlNode.Inline(tag, attrs.flatMap(_.toAttributes), children.flatMap(_.toNodes))

  def empty(attrs: HtmlAttributable*) =
    HtmlNode.Empty(tag, attrs.flatMap(_.toAttributes))

  def pre(attrs: HtmlAttributable*)(children: HtmlNodeable*) =
    HtmlNode.Pre(tag, attrs.flatMap(_.toAttributes), children.flatMap(_.toNodes))
}

object HtmlFu {
  import scala.language.implicitConversions

  implicit class StringHtmlFu(val tag: String) extends HtmlFu

  implicit class SymbolHtmlFu(val sym: Symbol) extends HtmlFu {
    protected def tag: String = sym.name
  }
}

