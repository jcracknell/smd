package smd
package util

/** Facility for encoding [[java.lang.String]] and [[scala.Char]] values to an equivalent valid representation as
  * a quoted Scala literal.
  *
  * Implementations will escape any character not in the range `~` (tilde) through ` ` (space) for clarity.
  */
object LiteralEncoding {
  private val Hex = "0123456789abcdef".toArray
  private val StringEscapes   = Array.fill('\\'.toInt + 1)('\u0000')
  private val CharEscapes     = Array.fill('\\'.toInt + 1)('\u0000')

  {
    val baseEscapes = Map(
      '\b' -> 'b',  // backspace
      '\t' -> 't',  // tab
      '\n' -> 'n',  // linefeed
      '\f' -> 'f',  // formfeed
      '\r' -> 'r',  // carriage return
      '\\' -> '\\'  // backslash
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
        Hex(ci >>> 12 & 0xF),
        Hex(ci >>>  8 & 0xF),
        Hex(ci >>>  4 & 0xF),
        Hex(ci        & 0xF),
        '\''
      )
    })

  /** Encode the provided [[scala.Char]] as a Scala character literal, enclosing the character in single quotes and replacing
    * the character with an escape version if necessary.
    *
    * @param c the [[scala.Char]] to be encoded as an equivalent character literal.
    * @param sb the [[scala.collection.mutable.StringBuilder]] to which the literal representation of the provided [[scala.Char]] should be appended.
    */
  def encode(c: Char, sb: StringBuilder): StringBuilder = {
    val ci = c.toInt
    sb.append('\'')
    if(ci < CharEscapes.length && '\u0000' != CharEscapes(ci))
      sb.append('\\').append(CharEscapes(ci))
    else if('~' >= c && c >= ' ')
      sb.append(c)
    else
      sb.append('\\').append('u')
        .append(Hex(ci >>> 12 & 0xF))
        .append(Hex(ci >>>  8 & 0xF))
        .append(Hex(ci >>>  4 & 0xF))
        .append(Hex(ci        & 0xF))
    sb.append('\'')
  }

  /** Encode the provided [[java.lang.CharSequence]] as a Scala string literal, enclosing the string in double quotes and replacing
    * characters with escape sequences as necessary.
    *
    * @param str the [[java.lang.CharSequence]] to be encoded as an equivalent string literal.
    */
  def encode(str: CharSequence): String = {
    if(null == str) return "null";

    encode(str, new StringBuilder(str.length + 2)).toString
  }

  /** Encode the provided [[java.lang.CharSequence]] as a Scala string literal, enclosing the string in double quotes and replacing
    * characters with escape sequences as necessary.
    *
    * @param str the [[java.lang.CharSequence]] to be encoded as an equivalent string literal.
    * @param sb the [[scala.collection.mutable.StringBuilder]] to which the literal representation of the provided string should be appended.
    * @return the provided [[scala.collection.mutable.StringBuilder]].
    */
  def encode(str: CharSequence, sb: StringBuilder): StringBuilder = {
    if(null == str) return sb.append("null");

    sb.append('"')
    var i = 0
    while(i < str.length) {
      val c = str.charAt(i)
      val ci = c.toInt

      if(ci < StringEscapes.length && '\u0000' != StringEscapes(ci))
        sb.append('\\').append(StringEscapes(ci))
      else if('~' >= c && c >= ' ')
        sb.append(c)
      else
        sb.append('\\').append('u')
          .append(Hex(ci >>> 12 & 0xF))
          .append(Hex(ci >>>  8 & 0xF))
          .append(Hex(ci >>>  4 & 0xF))
          .append(Hex(ci        & 0xF))
      i += 1
    }
    sb.append('"')
  }
}
