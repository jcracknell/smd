package smd
package util

object HtmlUtils {
  // http://tools.ietf.org/html/rfc3986
  private val utf8Charset = java.nio.charset.Charset.forName("UTF-8")
  private val hex = "0123456789ABCDEF".toCharArray

  val unreserved = {
    val unreservedChars = Set(
      'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm',
      'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z',
      'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M',
      'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z',
      '0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
      '-', '.', '_', '~'
    )
    unreservedChars map { _.toByte }
  }

  def encodeUriComponent[A <: Appendable](str: String, out: A): A = {
    val encoded = utf8Charset.encode(str)
    while(encoded.hasRemaining) {
      val b = encoded.get
      if(unreserved.contains(b))
        out.append(b.toChar)
      else
        out.append('%').append(hex(b >>> 4 & 0xF)).append(hex(b & 0xF))
    }
    out
  }

  def encodeUriComponent(str: String): String =
    encodeUriComponent(str, new java.lang.StringBuilder(str.length << 1)).toString
}
