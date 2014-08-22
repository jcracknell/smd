package smd
package parsing

case class LongestChoiceParser[+A](choices: IndexedSeq[Parser[A]]) extends Parser[A] {
  require(choices.lengthGte(2), "choices must contain at least two options.")

  def parse(context: ParsingContext): ParsingResult[A] = {
    var i = 0
    var bestLength = -1
    var bestResult: ParsingResult[A] = null
    do {
      val choiceResult = choices(i).parse(context.copy)
      if(choiceResult.accepted && choiceResult.length > bestLength) {
        bestResult = choiceResult
        bestLength = choiceResult.length
      }
      i += 1
    } while(i < choices.length)

    if(bestLength >= 0) {
      context.advanceTo(bestResult.endIndex)
      bestResult
    } else {
      Rejected
    }
  }
}

object LongestChoiceParser {
  def apply[A](cs: Seq[Parser[A]]): LongestChoiceParser[A] = apply(cs.toIndexedSeq)
  def apply[A](c0: Parser[A], c1: Parser[A], cns: Parser[A]*): LongestChoiceParser[A] = apply((c0 +: c1 +: cns).toIndexedSeq)
}
