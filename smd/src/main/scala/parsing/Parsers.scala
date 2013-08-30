package smd
package parsing

trait Parsers {
  protected val Grapheme =  smd.unicode.GraphemeCriteria
  protected val CodePoint = smd.unicode.CodePointCriteria

  def &:[A](parser: Parser[A]): AndPredicateParser[A] = AndPredicateParser(parser)

  def !:(parser: Parser[Any]): NotPredicateParser = NotPredicateParser(parser)

  /** The world's most useless implicit conversion.
    * This is necessary so that there is always an implicit conversion available in heuristic-based parser methods.
    */
  implicit def parser2Parser[A <: Parser[_]](parser: A): A = parser

  implicit def string2LiteralParser(str: String): LiteralParser = LiteralParser(str)

  implicit def regex2RegexParser(regex: scala.util.matching.Regex): RegexParser = RegexParser(regex)

  implicit def codePointCriterion2GraphemeParser(criterion: smd.unicode.CodePointCriterion): GraphemeParser =
    GraphemeParser(Grapheme.SingleCodePoint(criterion))

  implicit def graphemeCriterion2GraphemeParser(criterion: smd.unicode.GraphemeCriterion): GraphemeParser =
    GraphemeParser(criterion)
}
