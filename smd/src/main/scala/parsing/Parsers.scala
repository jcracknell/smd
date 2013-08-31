package smd
package parsing

trait Parsers {
  protected val Grapheme =  smd.unicode.GraphemeCriteria
  protected val CodePoint = smd.unicode.CodePointCriteria

  val EOF = EndOfInputParser

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

  implicit def range2GraphemeParser[A <% Int](range: collection.immutable.NumericRange[A]): GraphemeParser =
    GraphemeParser(Grapheme.SingleCodePoint(CodePoint.Range(range.start, range.last)))

  implicit def iterable2GraphemeParser[A <% Int](codePoints: Iterable[A]): GraphemeParser =
    GraphemeParser(Grapheme.SingleCodePoint(CodePoint.Values(codePoints)))
}
