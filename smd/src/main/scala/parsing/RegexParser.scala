package smd
package parsing

import scala.util.matching.Regex
import scala.util.matching.Regex.Match

case class RegexParser(regex: Regex) extends Parser[Regex.Match] {
  def parse(context: ParsingContext): ParsingResult[Match] = {
    // TODO: Verify this is our most performant option for matching at a specific index
    val remainingInput = context.input.slice(context.index, context.input.length)
    val matchResult = regex.findPrefixMatchOf(remainingInput)
    if(matchResult.isDefined) {
      // TODO: Need to verify that the last grapheme was matched in its entirety
      val mark = context.mark
      context.advanceBy(matchResult.get.end)
      mark.success(matchResult.get)
    } else {
      Failure
    }
  }
}

object RegexParser {
  def apply(pattern: String): RegexParser = RegexParser(pattern.r)
}
