package smd
package parsing

case object EndOfInputParser extends Parser[Unit] {
  def parse(context: ParsingContext): ParsingResult[Unit] = {
    val rb = context.resultBuilder
    if(context.index == context.input.length()) rb.success(()) else rb.failure
  }
}
