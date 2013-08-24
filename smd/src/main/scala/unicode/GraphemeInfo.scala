package smd
package unicode

/** Information about a Unicode grapheme.
  * The Unicode Consortium defines a grapheme as a "minimally distinctive unit of writing in cont of a
  * particular writing system", or more succinctly as "what end users think of as characters".
  *
  * @param category the Unicode category to which the grapheme belongs, as defined by [[java.lang.Character]].
  * @param length the length of the grapheme.
  */
final case class GraphemeInfo(category: Byte, index: Int, length: Int)

object GraphemeInfo {
  /** Retrieves [[smd.unicode.GraphemeInfo]] for the grapheme starting at the specified index in the provided string.
    *
    * @param str the string from which a grapheme's information should be retrieved.
    * @param index the start index of the grapheme for which information should be retrieved.
    * @return [[smd.unicode.GraphemeInfo]] for the grapheme starting at the specified index in the provided string.
    */
  def at(str: CharSequence, index: Int): GraphemeInfo = {
    // TODO: review whether or not the implementation of this method is correct per
    //       http://www.unicode.org/reports/tr29/tr29-17.html#Grapheme_Cluster_Boundaries
    var cp = CodePointInfo.at(str, index)

    // The category is determined by the first codepoint
    val category = cp.category

    // Can we add combining marks to the initial character?
    if(
      category != Character.FORMAT &&
      category != Character.CONTROL &&
      category != Character.SURROGATE &&
      !isCombiningCategory(category) &&
      category != Character.UNASSIGNED
    ) {
      // Read any combining marks following the initial character.
      var i = index + cp.length
      val strLength = str.length
      while(strLength != i) {
        cp = CodePointInfo.at(str, i)
        if(isCombiningCategory(cp.category)) {
          i += cp.length
        } else {
          return GraphemeInfo(category, index, i - index)
        }
      }
      return GraphemeInfo(category, index, i - index)
    }

    GraphemeInfo(category, index, cp.length)
  }

  @inline private def isCombiningCategory(category: Byte): Boolean =
    category == Character.NON_SPACING_MARK ||
    category == Character.COMBINING_SPACING_MARK ||
    category == Character.ENCLOSING_MARK
}
