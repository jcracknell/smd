package smd
package parsing

case class OrderedChoiceParser[+A](choices: IndexedSeq[Parser[A]]) extends Parser[A] {
  require(choices.lengthGte(2), "choices must contain at least two options.")

  def parse(context: ParsingContext): ParsingResult[A] = {
    var i = 0
    do {
      val choiceContext = context.copy
      val choiceResult = choices(i).parse(choiceContext)
      if(choiceResult.succeeded) {
        context.advanceBy(choiceResult.length)
        return choiceResult
      }

      i += 1
    } while(i < choices.length)

    ParsingResult.Failure
  }
}

object OrderedChoiceParser {
  def apply[A](c0: Parser[A], cns: Parser[A]*): OrderedChoiceParser[A] =
    OrderedChoiceParser((c0 +: cns).toIndexedSeq)

  def orderedChoiceHeuristic[A]: OrderedChoiceHeuristic[OrderedChoiceParser[A], Parser[A], OrderedChoiceParser[A]] =
    OrderedChoiceHeuristic.create((l, r) => OrderedChoiceParser(l.choices :+ r))
}