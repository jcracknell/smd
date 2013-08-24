package smd
package unicode

/** Information about a Unicode code point.
  *
  * @param codePoint the value of the code point.
  * @param category the Unicode category to which the code point belongs; see [[java.lang.Character]].
  * @param length the length of the code point.
  */
final case class CodePointInfo(codePoint: Int, category: Byte, length: Int)

object CodePointInfo {
  /** Retrieves [[smd.unicode.CodePointInfo]] for the Unicode code point at the specified index in the provided string.
    *
    * @param str the string from which [[smd.unicode.CodePointInfo]] should be retrieved.
    * @param index the index of the code point for which [[smd.unicode.CodePointInfo]] should be retrieved.
    * @return
    */
  def at(str: CharSequence, index: Int): CodePointInfo = {
    val hi = str.charAt(index)
    if(Character.MIN_HIGH_SURROGATE <= hi && hi <= Character.MAX_HIGH_SURROGATE && str.length > index + 1) {
      val lo = str.charAt(index + 1)
      if(Character.MIN_LOW_SURROGATE <= lo && lo <= Character.MAX_LOW_SURROGATE) {
        val cp = 0x10000 + (((hi - Character.MIN_HIGH_SURROGATE) << 10) | (lo - Character.MIN_LOW_SURROGATE))
        return CodePointInfo(cp, Character.getType(cp).toByte, 2)
      }
    }

    CodePointInfo(hi, Character.getType(hi).toByte, 1)
  }
}
