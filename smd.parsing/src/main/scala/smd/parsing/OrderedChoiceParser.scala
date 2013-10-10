package smd
package parsing

case class OrderedChoiceParser[+A](choices: IndexedSeq[Parser[A]]) extends Parser[A] {
  require(choices.lengthGte(2), "choices must contain at least two options.")

  def parse(context: ParsingContext): ParsingResult[A] = {
    var i = 0
    do {
      val choiceContext = context.copy
      val choiceResult = choices(i).parse(choiceContext)
      if(choiceResult.accepted) {
        context.advanceBy(choiceResult.length)
        return choiceResult
      }

      i += 1
    } while(i < choices.length)

    Rejected
  }
}

object OrderedChoiceParser {
  def apply[A](choices: Iterable[Parser[A]]): OrderedChoiceParser[A] = apply(choices.toIndexedSeq)

  def apply[A](c0: Parser[A], c1: Parser[A], cns: Parser[A]*): OrderedChoiceParser[A] = apply((c0 +: c1 +: cns).toIndexedSeq)
}
