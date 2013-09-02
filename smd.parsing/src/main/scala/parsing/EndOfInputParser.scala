package smd
package parsing

case object EndOfInputParser extends Parser[Nothing] {
  def parse(context: ParsingContext): ParsingResult[Nothing] = {
    val rb = context.resultBuilder
    if(context.index == context.input.length()) rb.nothing else rb.failure
  }
}
