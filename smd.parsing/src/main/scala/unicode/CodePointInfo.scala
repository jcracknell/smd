package smd
package unicode

/** Information about a Unicode code point.
  *
  * @param value the value of the code point.
  */
final case class CodePointInfo(value: Int) {
  assert(Character.isValidCodePoint(value), s"provided value ($value) is not a valid code point")

  /** The unicode category to which the code point belongs; see [[java.lang.Character]]. */
  val category: Byte = Character.getType(value).toByte

  /** The length of the code point as a UTF-16 string. */
  def length: Int = if(Character.MIN_SUPPLEMENTARY_CODE_POINT > value) 1 else 2
}

object CodePointInfo {
  /** Retrieves [[smd.unicode.CodePointInfo]] for the Unicode code point at the specified index in the provided string.
    *
    * @param str the string from which [[smd.unicode.CodePointInfo]] should be retrieved.
    * @param index the index of the code point for which [[smd.unicode.CodePointInfo]] should be retrieved.
    * @return
    */
  def at(str: CharSequence, index: Int): CodePointInfo =
    new CodePointInfo(Character.codePointAt(str, index))

  /** Retrieve [[smd.unicode.CodePointInfo]] for the provide code point. */
  def of(codePoint: Int): CodePointInfo = new CodePointInfo(codePoint)

  /** Retrieve [[smd.unicode.CodePointInfo]] for the provided [[java.lang.CharSequence]] representation of a code
    * point. */
  def of(codePoint: CharSequence): CodePointInfo = {
    val cpi = CodePointInfo.at(codePoint, 0)
    require(cpi.length == codePoint.length, s"provided code point is invalid")
    cpi
  }
}
