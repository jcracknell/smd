package smd
package util

/** Facility for encoding [[java.lang.String]] and [[scala.Char]] values to an equivalent valid representation as
  * a quoted Scala literal.
  *
  * Implementations will escape any character not in the range `~` (tilde) through ` ` (space) for clarity.
  */
object LiteralEncoding {
  private val StringEscapes   = Array.fill('\\'.toInt + 1)('\u0000')
  private val CharEscapes     = Array.fill('\\'.toInt + 1)('\u0000')

  {
    val baseEscapes = Map(
      '\b' -> 'b',  // backspace
      '\t' -> 't',  // tab
      '\n' -> 'n',  // linefeed
      '\f' -> 'f',  // formfeed
      '\r' -> 'r',  // carriage return
      '\\' -> '\\' // backslash
    )

    (baseEscapes + ('\"' -> '\"')).foreach { case (c, e) => StringEscapes(c.toInt) = e }
    (baseEscapes + ('\'' -> '\'')).foreach { case (c, e) => CharEscapes(c.toInt) = e }
  }

  /** Encode the provided [[scala.Char]] as a Scala character literal, enclosing the character in single quotes and replacing
    * the character with an escape version if necessary.
    *
    * @param c the [[scala.Char]] to be encoded as an equivalent character literal.
    * @return the literal representation of the provided [[scala.Char]].
    */
  def encode(c: Char): String =
    new String({
      val ci = c.toInt

      if(ci < CharEscapes.length && '\u0000' != CharEscapes(ci))
        Array('\'', '\\', CharEscapes(ci), '\'')
      else if('~' >= c && c >= ' ')
        Array('\'', c, '\'')
      else Array(
        '\'', '\\', 'u',
        HexEncoding.encodeDigitLower(ci >>> 12),
        HexEncoding.encodeDigitLower(ci >>>  8),
        HexEncoding.encodeDigitLower(ci >>>  4),
        HexEncoding.encodeDigitLower(ci),
        '\''
      )
    })

  /** Encode the provided [[scala.Char]] as a Scala character literal, enclosing the character in single quotes and replacing
    * the character with an escape version if necessary.
    *
    * @param c the [[scala.Char]] to be encoded as an equivalent character literal.
    * @param sb the [[scala.collection.mutable.StringBuilder]] to which the literal representation of the provided [[scala.Char]] should be appended.
    * @return the provided [[scala.collection.mutable.StringBuilder]].
    */
  def encode(c: Char, sb: StringBuilder): StringBuilder = {
    val ci = c.toInt
    sb.append('\'')
    if(ci < CharEscapes.length && '\u0000' != CharEscapes(ci))
      sb.append('\\').append(CharEscapes(ci))
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
      val ci = c.toInt

      if(ci < StringEscapes.length && '\u0000' != StringEscapes(ci))
        sb.append('\\').append(StringEscapes(ci))
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
