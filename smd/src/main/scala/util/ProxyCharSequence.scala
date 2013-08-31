package smd
package util

/** Ultra-lightweight implementation of [[java.lang.CharSequence]] which proxies to the provided underlying
  * instance instead of copying character data.
  * This implementation is as lazy as possible and performs no error checking whatsoever - you have been warned.
  *
  * @param underlying the underlying [[java.lang.CharSequence]].
  * @param start the start index of proxied range from the underlying sequence, inclusive.
  * @param end the end index of the proxied range from the underlying sequence, exclusive.
  */
class ProxyCharSequence(
  protected val underlying: CharSequence,
  protected val start: Int,
  protected val end: Int
) extends CharSequence {
  @inline private def mapIndex(i: Int) = start + i

  def length(): Int = end - start

  def charAt(i: Int): Char = underlying.charAt(mapIndex(i))

  def subSequence(from: Int, to: Int): CharSequence =
    new ProxyCharSequence(underlying, mapIndex(from), mapIndex(to))

  override def hashCode(): Int = underlying.hashCode() ^ start ^ end

  override def equals(obj: Any): Boolean = obj match {
    case that: ProxyCharSequence =>
      this.start == that.start && this.end == that.end && this.underlying == that.underlying
    case that: CharSequence =>
      this.length() == that.length() &&
      (0 until length).forall(i => underlying.charAt(mapIndex(i)) == that.charAt(i))
    case _ => false
  }


  override def toString: String =
    new String(Array.tabulate[Char](length)(i => underlying.charAt(mapIndex(i))))
}
