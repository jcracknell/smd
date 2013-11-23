package smd
package util

object HtmlUtils {
  // http://tools.ietf.org/html/rfc3986
  private val utf8Charset = java.nio.charset.Charset.forName("UTF-8")
  private val hex = "0123456789ABCDEF".toCharArray

  private val unreserved = {
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

  /** Percent-encodes the provided string as a valid URI component, replacing all reserved characters
    * per RFC 3986. */
  def encodeUriComponent(str: String, put: Char => Unit): Unit = {
    val encoded = utf8Charset.encode(str)
    while(encoded.hasRemaining) {
      val b = encoded.get
      if(unreserved contains b) {
        put(b.toChar)
      } else {
        put('%')
        put(hex(b >>> 4 & 0xF))
        put(hex(b       & 0xF))
      }
    }
  }

  /** Percent-encodes the provided string as a valid URI component, replacing all reserved characters
    * per RFC 3986. */
  def encodeUriComponent(str: String): String = {
    val sb = new StringBuilder(str.length << 1)
    encodeUriComponent(str, c => sb.append(c))
    sb.toString
  }

  private val htmlSpecialChars = Map(
    '<'  -> "&lt;",
    '>'  -> "&gt;",
    '&'  -> "&amp;",
    '"'  -> "&quot;",
    '\'' -> "&apos;"
  )

  /** HTML-encodes the provided string, replacing occurrences of the characters `<`, `>`, '&', `"` and `'` with
    * the equivalent named character entity. Other characters are unaffected and should be handled at the document
    * encoding level.
    */
  def htmlEncode(str: CharSequence, put: Char => Unit): Unit = {
    var i = 0
    while(i < str.length) {
      val c = str.charAt(i)
      htmlSpecialChars.get(c) match {
        case None => put(c)
        case Some(e) => e foreach put
      }
      i += 1
    }
  }

  /** HTML-encodes the provided string, replacing occurrences of the characters `<`, `>`, '&', `"` and `'` with
    * the equivalent named character entity. Other characters are unaffected and should be handled at the document
    * encoding level.
    */
  def htmlEncode(str: CharSequence): String = {
    val sb = new StringBuilder(str.length << 1)
    htmlEncode(str, c => sb.append(c))
    sb.toString
  }
}
