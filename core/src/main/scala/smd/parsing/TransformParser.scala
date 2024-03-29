package smd
package parsing

case class TransformParser[A, +B](parser: Parser[A], transform: ParsingResult[A] => B) extends Parser[B] {
  def parse(context: ParsingContext): ParsingResult[B] = {
    val r = parser.parse(context)
    if(r.accepted) r.copy(transform(r)) else Rejected
  }
}

object TransformParser {
  def product[A, B](parser: Parser[A], transform: A => B): TransformParser[A, B] =
    TransformParser(parser, r => transform(r.product))
}

