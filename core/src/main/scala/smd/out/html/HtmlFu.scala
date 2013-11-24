package smd
package out
package html

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

class HtmlFu(writer: HtmlWriter, tag: String) {
  def block(attrs: HtmlAttributable*)(content: => Unit): Unit =
    writer.writeBlock(tag, attrs.flatMap(_.toAttributes): _*)(content)

  def block(content: => Unit): Unit =
    writer.writeBlock(tag)(content)

  def inline(attrs: HtmlAttributable*)(content: => Unit): Unit =
    writer.writeSpan(tag, attrs.flatMap(_.toAttributes): _*)(content)

  def inline(content: => Unit): Unit =
    writer.writeSpan(tag)(content)

  def empty(a0: HtmlAttributable, attrs: HtmlAttributable*): Unit =
    writer.writeEmpty(tag, (a0 +: attrs).flatMap(_.toAttributes): _*)

  def empty(): Unit =
    writer.writeEmpty(tag)

  def pre(attrs: HtmlAttributable*)(content: => Unit): Unit =
    writer.writePre(tag, attrs.flatMap(_.toAttributes): _*)(content)

  def pre(content: => Unit): Unit =
    writer.writePre(tag)(content)
}

