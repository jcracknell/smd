package smd.out.html

import smd.util.HtmlUtils

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
