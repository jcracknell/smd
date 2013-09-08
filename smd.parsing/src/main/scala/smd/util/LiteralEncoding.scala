package smd
package util

/** Facility for encoding [[java.lang.String]] and [[scala.Char]] values to an equivalent valid representation as
  * a quoted Scala literal.
  *
  * Implementations will escape any character not in the range `~` (tilde) through ` ` (space) for clarity.
  */
object LiteralEncoding {
  private val MaxStringEscape = '\\'
  private val MaxCharEscape   = '\\'
  private val StringEscapes   = Array.fill(MaxStringEscape + 1)('\0')
  private val CharEscapes     = Array.fill(MaxCharEscape + 1)('\0')

  {
    val baseEscapes = Map(
      '\b' -> 'b',  // backspace
      '\t' -> 't',  // tab
      '\n' -> 'n',  // linefeed
      '\f' -> 'f',  // formfeed
      '\r' -> 'r',  // carriage return
      '\\' -> '\\', // backslash
      '\0' -> '0'   // null
    )

    (baseEscapes + ('\"' -> '\"')).foreach { case (c, e) => StringEscapes(c) = e }
    (baseEscapes + ('\'' -> '\'')).foreach { case (c, e) => CharEscapes(c) = e }
  }

  /** Encode the provided [[scala.Char]] as a Scala character literal, enclosing the character in single quotes and replacing
    * the character with an escape version if necessary.
    *
    * @param c the [[scala.Char]] to be encoded as an equivalent character literal.
    * @return the literal representation of the provided [[scala.Char]].
    */
  def encode(c: Char): String =
    new String(
      if(c <= MaxCharEscape && '\0' != CharEscapes(c))
        Array('\'', '\\', CharEscapes(c), '\'')
      else if('~' >= c && c >= ' ')
        Array('\'', c, '\'')
      else Array(
        '\'', '\\', 'u',
        HexEncoding.encodeDigitLower(c >>> 12),
        HexEncoding.encodeDigitLower(c >>>  8),
        HexEncoding.encodeDigitLower(c >>>  4),
        HexEncoding.encodeDigitLower(c),
        '\''
      )
    )

  /** Encode the provided [[scala.Char]] as a Scala character literal, enclosing the character in single quotes and replacing
    * the character with an escape version if necessary.
    *
    * @param c the [[scala.Char]] to be encoded as an equivalent character literal.
    * @param sb the [[scala.collection.mutable.StringBuilder]] to which the literal representation of the provided [[scala.Char]] should be appended.
    * @return the provided [[scala.collection.mutable.StringBuilder]].
    */
  def encode(c: Char, sb: StringBuilder): StringBuilder = {
    sb.append('\'')
    if(c <= MaxCharEscape && '\0' != CharEscapes(c))
      sb.append('\\').append(CharEscapes(c))
    else if('~' >= c && c >= ' ')
      sb.append(c)
    else {
      sb.append('\\').append('u')
      HexEncoding.encodeLower(c.toShort, sb)
    }
    sb.append('\'')
  }

  /** Encode the provided [[java.lang.String]] as a Scala string literal, enclosing the string in double quotes and replacing
    * characters with escape sequences as necessary.
    *
    * @param str the [[java.lang.String]] to be encoded as an equivalent string literal.
    * @return the literal representation of the provided [[java.lang.String]].
    */
  def encode(str: String): String = {
    if(null == str) return "null";

    val sb = new StringBuilder(str.length + 2)
    encode(str, sb)
    sb.toString
  }

  /** Encode the provided [[java.lang.String]] as a Scala string literal, enclosing the string in double quotes and replacing
    * characters with escape sequences as necessary.
    *
    * @param str the [[java.lang.String]] to be encoded as an equivalent string literal.
    * @param sb the [[scala.collection.mutable.StringBuilder]] to which the literal representation of the provided string should be appended.
    * @return the provided [[scala.collection.mutable.StringBuilder]].
    */
  def encode(str: String, sb: StringBuilder): StringBuilder = {
    if(null == str) return sb.append("null");

    sb.append('"')
    for(c <- str) {
      if(c <= MaxStringEscape && '\0' != StringEscapes(c))
        sb.append('\\').append(StringEscapes(c))
      else if('~' >= c && c >= ' ')
        sb.append(c)
      else {
        sb.append('\\').append('u')
        HexEncoding.encodeLower(c.toShort, sb)
      }
    }
    sb.append('"')
  }
}
