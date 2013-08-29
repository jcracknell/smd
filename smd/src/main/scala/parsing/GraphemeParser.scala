package smd
package parsing

import smd.unicode.{GraphemeInfo, GraphemeCriterion}

case class GraphemeParser(criterion: GraphemeCriterion) extends Parser[GraphemeInfo] {
  def parse(context: ParsingContext): ParsingResult[GraphemeInfo] = {
    val grapheme = GraphemeInfo.at(context.input, context.index)
    if(criterion.isSatisfiedBy(grapheme)) {
      val startIndex = context.index
      context.advanceBy(grapheme.length)
      Success(grapheme, startIndex, grapheme.length)
    } else {
      Failure
    }
  }
}
