package smd
package parsing

trait Parsers {
  protected val CodePoint =       smd.unicode.CodePointCriteria
  protected val Grapheme =        smd.unicode.GraphemeCriteria
  protected val UnicodeCategory = smd.unicode.UnicodeCategory

  val EOF = EndOfInputParser

  def &:[A](parser: Parser[A]): AndPredicateParser[A] = AndPredicateParser(parser)

  def !:(parser: Parser[Any]): NotPredicateParser = NotPredicateParser(parser)

  implicit def convertCategory2GraphemeParser(category: smd.unicode.UnicodeCategory): GraphemeParser =
    GraphemeParser(Grapheme.SingleCodePoint(CodePoint.Category(category)))

  implicit def convertCategories2GraphemeParser(categories: collection.Iterable[smd.unicode.UnicodeCategory]): GraphemeParser =
    GraphemeParser(Grapheme.SingleCodePoint(CodePoint.Category(categories)))

  implicit def convertCodePoint2GraphemeParser[A <% Int](codePoint: A): GraphemeParser =
    GraphemeParser(Grapheme.SingleCodePoint(CodePoint.Values(codePoint)))

  implicit def convertCodePointCriterion2GraphemeParser(criterion: smd.unicode.CodePointCriterion): GraphemeParser =
    GraphemeParser(Grapheme.SingleCodePoint(criterion))

  implicit def convertCodePointRange2GraphemeParser[A <% Int](range: collection.immutable.NumericRange[A]): GraphemeParser =
    GraphemeParser(Grapheme.SingleCodePoint(CodePoint.Range(range.start, range.last)))

  implicit def convertCodePoints2GraphemeParser[A <% Int](codePoints: collection.Iterable[A]): GraphemeParser =
    GraphemeParser(Grapheme.SingleCodePoint(CodePoint.Values(codePoints)))

  implicit def convertGraphemeCriterion2GraphemeParser(criterion: smd.unicode.GraphemeCriterion): GraphemeParser =
    GraphemeParser(criterion)

  /** The world's most useless implicit conversion.
    * This is necessary so that there is always an implicit conversion available in heuristic-based parser methods.
    */
  implicit def convertParser2Parser[A <: Parser[_]](parser: A): A = parser

  implicit def convertRegex2RegexParser(regex: scala.util.matching.Regex): RegexParser = RegexParser(regex)

  implicit def convertString2LiteralParser(str: String): LiteralParser = LiteralParser(str)
}
