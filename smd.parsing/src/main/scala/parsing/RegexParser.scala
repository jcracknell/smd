package smd
package parsing

import scala.util.matching.Regex
import scala.util.matching.Regex.Match

case class RegexParser(regex: Regex) extends Parser[Regex.Match] {
  def parse(context: ParsingContext): ParsingResult[Match] = {
    // TODO: Verify this is our most performant option for matching at a specific index
    val rb = context.resultBuilder
    val remainingInput = context.input.proxySubSequence(context.index, context.input.length)
    val matchResult = regex.findPrefixMatchOf(remainingInput)
    if(matchResult.isDefined) {
      // TODO: Need to verify that the last grapheme was matched in its entirety
      context.advanceBy(matchResult.get.end)
      rb.success(matchResult.get)
    } else {
      rb.failure
    }
  }
}

object RegexParser {
  def apply(pattern: String): RegexParser = RegexParser(pattern.r)
}
