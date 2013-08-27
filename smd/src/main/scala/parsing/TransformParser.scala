package smd
package parsing

case class TransformParser[A, +B](parser: Parser[A], transform: ParsingResult[A] => B) extends Parser[B] {
  def parse(context: ParsingContext): ParsingResult[B] = {
    val r = parser.parse(context)
    if(r.succeeded) Success(transform(r), r.index, r.length) else Failure
  }
}

