package smd
package parsing

trait Parsers {
  implicit def string2LiteralParser(str: String): LiteralParser = LiteralParser(str)

  implicit def regex2RegexParser(regex: scala.util.matching.Regex): RegexParser = RegexParser(regex)

  def &:[A](parser: Parser[A]): AndPredicateParser[A] = AndPredicateParser(parser)

  def !:(parser: Parser[Any]): NotPredicateParser = NotPredicateParser(parser)
}
