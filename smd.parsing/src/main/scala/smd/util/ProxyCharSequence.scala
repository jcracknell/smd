package smd
package util

/** Ultra-lightweight implementation of [[java.lang.CharSequence]] which proxies to the provided underlying
  * instance instead of copying character data. This implementation is as lightweight as possible and performs
  * no error checking whatsoever.
  *
  * @param proxied the underlying [[java.lang.CharSequence]] to which this instance proxies.
  * @param proxiedStart the start index of proxied range from the underlying sequence, inclusive.
  * @param proxiedEnd the end index of the proxied range from the underlying sequence, exclusive.
  */
class ProxyCharSequence(
  val proxied: CharSequence,
  val proxiedStart: Int,
  val proxiedEnd: Int
) extends CharSequence {
  @inline private def mapIndex(i: Int) = proxiedStart + i

  private val _length = proxiedEnd - proxiedStart
  def length(): Int = _length

  def charAt(i: Int): Char = proxied.charAt(mapIndex(i))

  def subSequence(start: Int, end: Int): CharSequence =
    new ProxyCharSequence(proxied, mapIndex(start), mapIndex(end))

  override def hashCode(): Int = proxied.hashCode() ^ proxiedStart ^ proxiedEnd

  override def equals(obj: Any): Boolean = obj match {
    case that: CharSequence =>
      this.length() == that.length() &&
      (0 until _length).forall(i => this.charAt(i) == that.charAt(i))
    case _ => false
  }


  override def toString: String =
    new String(Array.tabulate(length)(i => charAt(i)))
}
