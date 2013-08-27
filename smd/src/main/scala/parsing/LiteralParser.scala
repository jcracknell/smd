package smd
package parsing

case class LiteralParser(literal: String) extends Parser[String] {
  import smd.unicode.GraphemeInfo

  if(0 == literal.length) throw new IllegalArgumentException(s"Provided literal ${literal.literalEncode} has length 0.")

  private val lastGrapheme = GraphemeInfo.iterable(literal).last

  def parse(context: ParsingContext): ParsingResult[String] = {
    var i = 0
    while(i != literal.length) {
      if(literal.charAt(i) != context.input.charAt(context.index + i))
        return Failure
      i += 1
    }

    // Having checked sequence equivalence, we must also verify that the final grapheme in the literal fully matches
    // the grapheme at the corresponding location in the input in order to handle the case where the matching section
    // in the input is followed by additional combining marks.
    val finalContextGrapheme = GraphemeInfo.at(context.input, context.index + lastGrapheme.index)
    if(lastGrapheme.length != finalContextGrapheme.length)
      return Failure

    val result = Success(literal, context.index, literal.length)
    context.advanceBy(literal.length)
    result
  }
}

object LiteralParser {
  /** Concatenation heuristic which combines adjacent literals. */
  // TODO: Is this a good idea, or is it too confusing? Certainly it has performance benefits.
  implicit val concatenationHeuristic = new ConcatenationHeuristic[LiteralParser, LiteralParser, LiteralParser] {
    def concat(lhs: LiteralParser, rhs: LiteralParser): LiteralParser =
      LiteralParser(lhs.literal + rhs.literal)
  }
}
