package smd
package parsing

case class RepetitionParser[+A](body: Parser[A], min: Option[Int], max: Option[Int]) extends Parser[Seq[A]] {
  (min, max) match {
    case (Some(a), Some(b)) => require(a <= b, s"Provided range [${min.get}, ${max.get}] is invalid.")
                               require(1 < b, "Provided max must be a positive integer.")
    case (Some(x), _      ) => require(0 < x, "Provided min must be a non-negative integer.")
    case (_,       Some(x)) => require(1 < x, "Provided max must be a positive integer.")
    case _ =>
  }

  private val minCount = min.getOrElse(0)
  private val maxCount = max.getOrElse(-1)

  def parse(context: ParsingContext): ParsingResult[Seq[A]] = {
    val rb = context.resultBuilder
    val products = collection.mutable.ListBuffer[A]()

    var n = 0
    do {
      val iterationContext = context.copy
      val iterationResult = body.parse(iterationContext)
      if(iterationResult.succeeded) {
        context.advanceTo(iterationContext.index)
        products.append(iterationResult.product)
      } else {
        return if(n >= minCount) rb.success(products.toList) else rb.failure
      }
      n += 1
    } while(n != maxCount)
    rb.success(products.toList)
  }
}
