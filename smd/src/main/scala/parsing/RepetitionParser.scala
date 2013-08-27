package smd
package parsing

case class RepetitionParser[+A](body: Parser[A], min: Option[Int], max: Option[Int]) extends Parser[Seq[A]] {
  (min, max) match {
    case (Some(a), Some(b)) if a > b => throw new IllegalArgumentException(s"Provided range [${min.get}, ${max.get}] is invalid.")
    case (Some(x), _) if x < 0 => throw new IllegalArgumentException("Provided min must be a non-negative integer.")
    case (_, Some(x)) if x < 0 => throw new IllegalArgumentException("Provided max must be a non-negative integer.")
  }

  private val minCount = min.getOrElse(0)
  private val maxCount = max.getOrElse(-1)

  def parse(context: ParsingContext): ParsingResult[Seq[A]] = {
    val mark = context.mark
    val products = collection.mutable.ListBuffer[A]()

    var n = 0
    while(true) {
      val iterationContext = context.clone
      val iterationResult = body.parse(iterationContext)

      if(iterationResult.succeeded) {
        context.assimilate(iterationContext)
        products.append(iterationResult.product)
        n += 1

        if(maxCount == n)
          return mark.success(products.toList)
      } else {
        return if(minCount > n) Failure else mark.success(products.toList)
      }
    }
    ???
  }
}
