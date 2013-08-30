package smd
package parsing

case class OrderedChoiceParser[+A](choices: Parser[A]*) extends Parser[A] {
  def parse(context: ParsingContext): ParsingResult[A] = {
    var i = 0
    while(i < choices.length) {
      val choiceContext = context.clone
      val choiceResult = choices(i).parse(choiceContext)
      if(choiceResult.succeeded) {
        context.assimilate(choiceContext)
        return choiceResult
      }

      i += 1
    }

    ParsingResult.Failure
  }
}

object OrderedChoiceParser {
  def orderedChoiceHeuristic[A]: OrderedChoiceHeuristic[OrderedChoiceParser[A], Parser[A], OrderedChoiceParser[A]] =
    OrderedChoiceHeuristic.create((l, r) => OrderedChoiceParser(l.choices :+ r: _*))
}
