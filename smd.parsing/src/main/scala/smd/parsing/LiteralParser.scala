package smd
package parsing

case class LiteralParser(literal: String) extends Parser[String] {
  import smd.unicode.GraphemeInfo

  if(0 == literal.length) throw new IllegalArgumentException(s"Provided literal ${literal.literalEncode} has length 0.")

  private val lastGrapheme = GraphemeInfo.iterable(literal).last

  def parse(context: ParsingContext): ParsingResult[String] = {
    val rb = context.resultBuilder

    if(context.input.length - context.index < literal.length)
      return rb.reject

    var i = 0
    do {
      if(literal.charAt(i) != context.input.charAt(context.index + i))
        return rb.reject
      i += 1
    } while(i != literal.length)

    // Having checked sequence equivalence, we must also verify that the final grapheme in the literal fully matches
    // the grapheme at the corresponding location in the input in order to handle the case where the matching section
    // in the input is followed by additional combining marks.
    val finalContextGrapheme = context.graphemeAt(context.index + lastGrapheme.start)
    if(lastGrapheme.length != finalContextGrapheme.length)
      return rb.reject

    context.advanceBy(literal.length)
    rb.accept(literal)
  }
}
