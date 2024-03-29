package smd
package parsing

import smd.unicode.{GraphemeInfo, GraphemeCriterion}

case class GraphemeParser(criterion: GraphemeCriterion) extends Parser[GraphemeInfo] {
  def parse(context: ParsingContext): ParsingResult[GraphemeInfo] = {
    val rb = context.resultBuilder
    if(context.index >= context.input.length)
      return rb.reject

    val grapheme = context.graphemeAt(context.index)
    if(criterion.isSatisfiedBy(grapheme)) {
      context.advanceBy(grapheme.length)
      rb.accept(grapheme)
    } else {
      rb.reject
    }
  }
}
