package smd
package parsing

case class LiteralParser(literal: String) extends Parser[String] {
  import smd.unicode.GraphemeInfo

  if(0 == literal.length) throw new IllegalArgumentException(s"Provided literal ${literal.literalEncode} has length 0.")

  private val lastGrapheme = GraphemeInfo.iterable(literal).last

  def parse(context: ParsingContext): ParsingResult[String] = {
    val rb = context.resultBuilder
    var i = 0
    while(i != literal.length) {
      if(literal.charAt(i) != context.input.charAt(context.index + i))
        return rb.failure
      i += 1
    }

    // Having checked sequence equivalence, we must also verify that the final grapheme in the literal fully matches
    // the grapheme at the corresponding location in the input in order to handle the case where the matching section
    // in the input is followed by additional combining marks.
    val finalContextGrapheme = GraphemeInfo.at(context.input, context.index + lastGrapheme.index)
    if(lastGrapheme.length != finalContextGrapheme.length)
      return rb.failure

    context.advanceBy(literal.length)
    rb.success(literal)
  }
}

object LiteralParser {
  // TODO: Is this a good idea, or is it too confusing? Certainly it has performance benefits.
  /** [[smd.parsing.SequencingHeuristic]] which combines adjacent [[smd.parsing.LiteralParser]] instances into a single
    * equivalent [[smd.parsing.LiteralParser]]. */
  implicit val sequencingHeuristic: SequencingHeuristic[LiteralParser, LiteralParser, LiteralParser] =
    SequencingHeuristic.create((l, r) => LiteralParser(l.literal + r.literal))

  /** [[smd.parsing.SequencingHeuristic]] which combines an [[smd.parsing.LiteralParser]] with and adjacent string into
    * a single equivalent [[smd.parsing.LiteralParser]]. */
  implicit val convertingSequencingHeuristic: SequencingHeuristic[LiteralParser, String, LiteralParser] =
    SequencingHeuristic.create((l, r) => LiteralParser(l.literal + r))
}
