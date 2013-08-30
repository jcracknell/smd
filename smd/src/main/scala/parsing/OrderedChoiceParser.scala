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

  override def ||[B >: A](rhs: Parser[B]): OrderedChoiceParser[B] =
    OrderedChoiceParser((choices :+ rhs):_*)
}
