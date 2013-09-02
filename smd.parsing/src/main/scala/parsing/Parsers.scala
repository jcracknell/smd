package smd
package parsing

trait Parsers {
  protected val CodePoint =       smd.unicode.CodePointCriteria
  protected val Grapheme =        smd.unicode.GraphemeCriteria
  protected val UnicodeCategory = smd.unicode.UnicodeCategory

  val EOF = EndOfInputParser

  def &:[A](parser: Parser[A]): AndPredicateParser[A] = AndPredicateParser(parser)

  def !:(parser: Parser[Any]): NotPredicateParser = NotPredicateParser(parser)

  implicit def convertCodePointCriterion2Parser(criterion: smd.unicode.CodePointCriterion): GraphemeParser =
    GraphemeParser(Grapheme.SingleCodePoint(criterion))

  implicit def convertCodePointCriterion2Implicit(criterion: smd.unicode.CodePointCriterion): Implicit[GraphemeParser] =
    Implicit(criterion)

  implicit def convertGraphemeCriterion2Parser(criterion: smd.unicode.GraphemeCriterion): GraphemeParser =
    GraphemeParser(criterion)

  implicit def convertGraphemeCriterion2Implicit(criterion: smd.unicode.GraphemeCriterion): Implicit[GraphemeParser] =
    Implicit(criterion)

  implicit def convertRegex2Parser(regex: scala.util.matching.Regex): RegexParser = RegexParser(regex)

  implicit def convertRegex2Implicit(regex: scala.util.matching.Regex): Implicit[RegexParser] =
    Implicit(regex)

  implicit def convertString2Parser(str: String): LiteralParser = LiteralParser(str)

  implicit def convertString2Implicit(str: String): Implicit[LiteralParser] =
    Implicit(str)

  implicit def convertUnicodeCategory2Parser(category: smd.unicode.UnicodeCategory): GraphemeParser =
    GraphemeParser(Grapheme.SingleCodePoint(CodePoint.Category(category)))

  implicit def convertUnicodeCategory2Implicit(category: smd.unicode.UnicodeCategory): Implicit[GraphemeParser] =
    Implicit(category)

  implicit def convertUnicodeCategories2Parser(categories: collection.Iterable[smd.unicode.UnicodeCategory]): GraphemeParser =
    GraphemeParser(Grapheme.SingleCodePoint(CodePoint.Category(categories)))

  implicit def convertUnicodeCategories2Implicit(categories: collection.Iterable[smd.unicode.UnicodeCategory]): Implicit[GraphemeParser] =
    Implicit(categories)
}
