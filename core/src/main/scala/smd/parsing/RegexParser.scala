package smd
package parsing

import scala.util.matching.Regex
import scala.util.matching.Regex.Match

case class RegexParser(regex: Regex) extends Parser[Regex.Match] {
  def parse(context: ParsingContext): ParsingResult[Match] = {
    // TODO: Verify this is our most performant option for matching at a specific index
    val rb = context.resultBuilder

    val remainingInput = context.input.subSequenceProxy(context.index, context.input.length)
    val matchResult = regex.findPrefixMatchOf(remainingInput)

    if(matchResult.isEmpty)
      return rb.reject

    val m = matchResult.get

    // The match must either be empty or finish a grapheme
    // Note that as we are prefix matching against a subsequence, m.end is the length of the match
    val matchEnd = context.index + m.end
    if(matchEnd == context.index || matchEnd == context.graphemeAt(matchEnd - 1).end) {
      context.advanceTo(matchEnd)
      rb.accept(m)
    } else {
      rb.reject
    }
  }
}

object RegexParser {
  def apply(pattern: String): RegexParser = RegexParser(pattern.r)
}
