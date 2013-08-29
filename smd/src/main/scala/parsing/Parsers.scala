package smd
package parsing

trait Parsers {
  protected val Grapheme =  smd.unicode.GraphemeCriteria
  protected val CodePoint = smd.unicode.CodePointCriteria

  implicit def string2LiteralParser(str: String): LiteralParser = LiteralParser(str)

  implicit def regex2RegexParser(regex: scala.util.matching.Regex): RegexParser = RegexParser(regex)

  implicit def graphemeCriterion2GraphemeParser(criterion: smd.unicode.GraphemeCriterion): GraphemeParser =
    GraphemeParser(criterion)

  implicit def codePointCriterion2GraphemeParser(criterion: smd.unicode.CodePointCriterion): GraphemeParser =
    GraphemeParser(Grapheme.SingleCodePoint(criterion))

  def &:[A](parser: Parser[A]): AndPredicateParser[A] = AndPredicateParser(parser)

  def !:(parser: Parser[Any]): NotPredicateParser = NotPredicateParser(parser)
}
