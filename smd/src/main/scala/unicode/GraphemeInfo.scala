package smd
package unicode

/** Information about a Unicode grapheme.
  * The Unicode Consortium defines a grapheme as a "minimally distinctive unit of writing in cont of a
  * particular writing system", or more succinctly as "what end users think of as characters".
  *
  * @param index the index of the grapheme in the source string.
  * @param length the number of constituent UTF-16 characters in the grapheme.
  * @param category the Unicode category to which the grapheme belongs, as defined by [[java.lang.Character]].
  */
final case class GraphemeInfo(index: Int, length: Int, category: Byte)

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
    var i = index + cp.length
    if(
      category != Character.FORMAT &&
      category != Character.CONTROL &&
      category != Character.SURROGATE &&
      !isCombiningCategory(category) &&
      category != Character.UNASSIGNED
    ) {
      // Read any combining marks following the initial character.
      while(str.length != i) {
        cp = CodePointInfo.at(str, i)
        if(isCombiningCategory(cp.category)) {
          i += cp.length
        } else {
          return GraphemeInfo(index, i - index, category)
        }
      }
    }

    GraphemeInfo(index, i - index, category)
  }

  def iterable(str: CharSequence): Iterable[GraphemeInfo] = iterable(str, 0)

  def iterable(str: CharSequence, index: Int): Iterable[GraphemeInfo] = new Iterable[GraphemeInfo] {
    def iterator: Iterator[GraphemeInfo] = new Iterator[GraphemeInfo] {
      private var i = index
      def hasNext: Boolean = i < str.length
      def next(): GraphemeInfo = {
        val g = GraphemeInfo.at(str, i)
        i += g.length
        g
      }
    }
  }

  @inline private def isCombiningCategory(category: Byte): Boolean =
    category == Character.NON_SPACING_MARK ||
    category == Character.COMBINING_SPACING_MARK ||
    category == Character.ENCLOSING_MARK
}
