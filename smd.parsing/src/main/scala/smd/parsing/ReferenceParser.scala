package smd
package parsing

/** [[smd.parsing.Parser]] implementation which lazily initializes a [[smd.parsing.Parser]] provided by-name.
  *
  * @param referenced the referenced parser.
  * @tparam A the product type of the referenced parser.
  */
class ReferenceParser[+A](referenced: => Parser[A]) extends Parser[A] {
  lazy val parser: Parser[A] = referenced

  def parse(context: ParsingContext): ParsingResult[A] =  parser.parse(context)
}

object ReferenceParser {
  def apply[A](reference: => Parser[A]): ReferenceParser[A] = new ReferenceParser[A](reference)
}
