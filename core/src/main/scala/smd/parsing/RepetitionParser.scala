package smd
package parsing

case class RepetitionParser[+A](body: Parser[A], min: Option[Int], max: Option[Int]) extends Parser[Seq[A]] {
  if(min.isDefined && max.isDefined)
    assert(min.get <= max.get, s"Provided range [${min.get}, ${max.get}] is invalid.")
  if(min.isDefined)
    assert(min.get >= 0, "Provided min must be a non-negative integer.")
  if(max.isDefined)
    assert(max.get >= 1, "Provided max must be a positive integer.")

  private val minCount = min.getOrElse(0)
  private val maxCount = max.getOrElse(-1)

  def parse(context: ParsingContext): ParsingResult[Seq[A]] = {
    val rb = context.resultBuilder
    val products = collection.mutable.ListBuffer[A]()

    var n = 0
    do {
      val iterationContext = context.copy
      val iterationResult = body.parse(iterationContext)
      if(iterationResult.accepted) {
        context.advanceTo(iterationContext.index)
        products.append(iterationResult.product)
      } else {
        return if(n >= minCount) rb.accept(products.toList) else rb.reject
      }
      n += 1
    } while(n != maxCount)
    rb.accept(products.toList)
  }
}
