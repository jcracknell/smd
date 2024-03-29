package smd
package unicode

/** Information about a Unicode grapheme found in a [[java.lang.CharSequence]].
  * The Unicode Consortium defines a grapheme as a "minimally distinctive unit of writing in cont of a
  * particular writing system", or more succinctly as "what end users think of as characters".
  *
  * @param source the [[java.lang.CharSequence]] from which the grapheme was extracted.
  * @param start the start index of the grapheme in the source, inclusive.
  * @param end the end index of the grapheme in the source, exclusive.
  * @param category the unicode category of the grapheme (see [[java.lang.Character]]).
  * @param codePoints the code points forming the grapheme.
  */
final class GraphemeInfo(
  protected val source: CharSequence,
  val start: Int,
  val end: Int,
  val category: Byte,
  val codePoints: Seq[CodePointInfo]
) {
  /** The length of the grapheme as a UTF-16 string. Note that this is not the number of code points in the grapheme. */
  def length: Int = end - start

  /** Retrieves a [[scala.collection.Seq]] containing the [[scala.Char]] values of the grapheme. */
  def chars: Seq[Char] = (start until end).map(source.charAt)

  /** Retrieves a [[java.lang.CharSequence]] containing the [[scala.Char]] values of the grapheme. */
  def charSequence: CharSequence = source.subSequenceProxy(start, end)

  override def hashCode(): Int = start ^ end ^ codePoints.hashCode()

  override def equals(obj: Any): Boolean = obj match {
    case that: GraphemeInfo =>
      this.start == that.start && this.end == that.end && this.codePoints == that.codePoints
    case _ => false
  }

  /** The string representation of the grapheme. */
  override def toString: String =
    new String(Array.tabulate[Char](length)(i => source.charAt(start + i)))
}

object GraphemeInfo {
  /** Extracts the start, length, category and code points from the provided [[smd.unicode.GraphemeInfo]]. */
  def unapply(g: GraphemeInfo): Option[(Int, Int, Byte, Seq[CodePointInfo])] =
    Some((g.start, g.length, g.category, g.codePoints))

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
    var codePoints = cp :: Nil

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
          codePoints :+= cp
        } else {
          return new GraphemeInfo(str, index, i, category, codePoints)
        }
      }
    }

    new GraphemeInfo(str, index, i, category, codePoints)
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

  /** Create a lookup which can be used to quickly retrieve the grapheme encompassing any index in `str`.
    *
    * @param str the [[java.lang.CharSequence]] for which a lookup should be created.
    * @return a lookup which can be used to quickly retriev the grapheme encompassing any index in `str`.
    */
  def createLookup(str: CharSequence): PartialFunction[Int, GraphemeInfo] = {
    val lut = Array.ofDim[GraphemeInfo](str.length())
    var i = 0
    while(i < str.length()) {
      val g = at(str, i)
      val j = i + g.length
      while(i < j) {
        lut(i) = g
        i += 1
      }
    }
    lut
  }

  @inline private def isCombiningCategory(category: Byte): Boolean =
    // This is a happy coincidence, as NSM = 6, EM = 7, CSM = 8
    // The order of comparisons is chosen because most characters (letters) will fail on the first one
    Character.NON_SPACING_MARK <= category && category <= Character.COMBINING_SPACING_MARK
}
