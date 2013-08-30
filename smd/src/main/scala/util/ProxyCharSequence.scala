package smd
package util

/** Ultra-lightweight implementation of [[java.lang.CharSequence]] which proxies to the provided underlying
  * instance instead of copying character data.
  * This implementation is as lazy as possible and performs no error checking whatsoever - you have been warned.
  *
  * @param underlying the underlying [[java.lang.CharSequence]].
  * @param start the start index of the range of the underlying [[java.lang.CharSequence]] accessible via this proxy.
  * @param end the end index of the range of the underlying [[java.lang.CharSequence]] accessible via this proxy.
  */
class ProxyCharSequence(
  protected val underlying: CharSequence,
  protected val start: Int,
  protected val end: Int
) extends CharSequence {

  def length(): Int = end - start

  def charAt(i: Int): Char = underlying.charAt(start + i)

  def subSequence(from: Int, to: Int): CharSequence =
    new ProxyCharSequence(underlying, start + from, start + to)
}
