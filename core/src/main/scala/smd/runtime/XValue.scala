package smd
package runtime

sealed abstract class XValue {
  def asBoolean: XBoolean
  def asNumber: XNumber
  def asString: XString
}

object XValue {
  implicit class XValueOps(private val x: XValue) extends AnyVal {
    // N.B. that in practically all cases, the desired behavior for operations involving numbers
    // is already provided by Scala/Java's Double implementation.
    def unary_! : XBoolean = XBoolean(!x.asBoolean.value)
    def unary_+ : XNumber = x.asNumber
    def unary_- : XNumber = XNumber(-x.asNumber.value)
    def + (y: XValue): XValue = (x, y) match {
      case (_: XString, _) | (_, _: XString) => XString(x.asString.value + y.asString.value)
      case _ => XNumber(x.asNumber.value + y.asNumber.value)
    }
    def - (y: XValue): XNumber = XNumber(x.asNumber.value - y.asNumber.value)
    def * (y: XValue): XNumber = XNumber(x.asNumber.value * y.asNumber.value)
    def / (y: XValue): XNumber = XNumber(x.asNumber.value / y.asNumber.value)
    def % (y: XValue): XNumber = XNumber(x.asNumber.value % y.asNumber.value)
    /** Exponentiation, NOT XOR. */
    def ^ (y: XValue): XNumber = XNumber(Math.pow(x.asNumber.value, y.asNumber.value))
    def && (y: XValue): XValue = if(x.asBoolean.value) y else x
    def || (y: XValue): XValue = if(x.asBoolean.value) x else y
  }
}

case class XArray(elements: XValue*) extends XValue {
  def asBoolean: XBoolean = XTrue
  def asNumber: XNumber = XNumber.NaN
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
  def asNumber: XNumber = XNumber.NaN
  def asString: XString = ???
}

case class XObject(properties: Map[String, XValue]) extends XValue {
  def asBoolean: XBoolean = XTrue
  def asNumber: XNumber = XNumber.NaN
  def asString: XString = XString("[object]")
}

case object XNull extends XValue {
  val asBoolean: XBoolean = XFalse
  val asNumber: XNumber = XNumber(0d)
  val asString: XString = XString("null")
}

sealed abstract class XNumber extends XValue {
  def value: Double
}

object XNumber {
  def apply(v: Double): XNumber = {
    if(Double.NegativeInfinity == v) return XNumber.NegInf
    if(Double.PositiveInfinity == v) return XNumber.PosInf
    if(v.asInstanceOf[Double] != v) return XNumber.NaN

    new XNumber.Value { val value: Double = v }
  }

  def apply(value: Float): XNumber = XNumber(value.toDouble)
  def apply(value: Int): XNumber = XNumber(value.toDouble)
  def apply(value: Short): XNumber = XNumber(value.toDouble)
  def apply(value: Byte): XNumber = XNumber(value.toDouble)

  sealed abstract class Value extends XNumber {
    def asNumber: XNumber = this
    def asString: XString = XString(value.toString)
    def asBoolean: XBoolean = XBoolean(value != 0)

    override def hashCode(): Int = value.hashCode()

    override def equals(obj: Any) = obj match {
      case that: Value => this.value == that.value
      case _ => false
    }

    override def toString: String = s"${classOf[Value].getName}($value)"
  }

  object Value {
    def unapply(v: Value): Option[Double] = v match {
      case v: Value => Some(v.value)
      case _ => None
    }
  }

  case object NaN extends XNumber {
    val value: Double = Double.NaN
    def asBoolean: XBoolean = XFalse
    def asNumber: XNumber = this
    val asString: XString = XString("NaN")
  }

  case object PosInf extends XNumber {
    val value: Double = Double.PositiveInfinity
    def asBoolean: XBoolean = XTrue
    def asNumber: XNumber = this
    val asString: XString = XString("PosInf")
  }

  case object NegInf extends XNumber {
    val value: Double = Double.NegativeInfinity
    def asBoolean: XBoolean = XFalse
    def asNumber: XNumber = this
    val asString: XString = XString("NegInf")
  }
}

case class XString(value: String) extends XValue {
  def asBoolean: XBoolean = XBoolean(value.length != 0)
  def asNumber: XNumber = try { XNumber(java.lang.Double.parseDouble(value)) } catch { case _: Throwable => XNumber.NaN.asNumber }
  def asString: XString = this
}

case object XUndefined extends XValue {
  val asBoolean: XBoolean = XFalse
  def asNumber: XNumber = XNumber.NaN
  val asString: XString = XString("undefined")
}


