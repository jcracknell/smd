package smd
package runtime

sealed abstract class XValue {
  def asBoolean: XBoolean
  def asNumber: XNumber
  def asString: XString
}

case class XArray(elements: XValue*) extends XValue {
  def asBoolean: XBoolean = XTrue
  def asNumber: XNumber = throw new Exception("Attempted to convert array value to number.")
  def asString: XString = XString(elements.map(_.asString.value).mkString("[",",","]"))
}

sealed abstract class XBoolean(val value: Boolean) extends XValue {
  def asBoolean: XBoolean = this
  val asNumber: XNumber = if(value) XNumber(1d) else XNumber(0d)
  val asString: XString = XString(value.toString)
}

object XBoolean {
  def apply(value: Boolean): XBoolean = if(value) XTrue else XFalse
  def unapply(value: XValue): Option[Boolean] = value match {
    case b: XBoolean => Some(b.value)
    case _ => None
  }
}

case object XTrue extends XBoolean(true)
case object XFalse extends XBoolean(false)

case class XDocument(nodes: dom.Markdown*) extends XValue {
  def asBoolean: XBoolean = XTrue
  def asNumber: XNumber = throw new Exception("Attempted to convert document value to number.")
  def asString: XString = ???
}

case class XObject(properties: Map[String, XValue]) extends XValue {
  def asBoolean: XBoolean = XTrue
  def asNumber: XNumber = throw new Exception("Attempted to convert object value to number.")
  def asString: XString = XString("[object]")
}

case object XNull extends XValue {
  val asBoolean: XBoolean = XFalse
  val asNumber: XNumber = XNumber(0d)
  val asString: XString = XString("null")
}

case class XNumber(value: Double) extends XValue {
  def asBoolean: XBoolean = XBoolean(value != 0d)
  def asNumber: XNumber = this
  def asString: XString = XString(value.toString)
}

object XNumber {
  def apply(value: Float): XNumber = XNumber(value.toDouble)
  def apply(value: Int): XNumber = XNumber(value.toDouble)
  def apply(value: Short): XNumber = XNumber(value.toDouble)
  def apply(value: Byte): XNumber = XNumber(value.toDouble)
}

case class XString(value: String) extends XValue {
  def asBoolean: XBoolean = XBoolean(value.length != 0)
  def asNumber: XNumber = XNumber(java.lang.Double.valueOf(value))
  def asString: XString = this
}

case object XUndefined extends XValue {
  val asBoolean: XBoolean = XFalse
  def asNumber: XNumber = throw new Exception("Attempted to convert undefined value to number.")
  val asString: XString = XString("undefined")
}


