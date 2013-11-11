package smd
package parsing

case class ParserMagnet[+P <: Parser[_]](parser: P) {
  def p: P = parser
}

object ParserMagnet {
  import scala.language.implicitConversions

  implicit def toParser[P <: Parser[_]](magnet: ParserMagnet[P]): P = magnet.parser

  /** Implicit conversion for the trivial [[smd.parsing.ParserMagnet]]. */
  implicit def fromParser[P <: Parser[_]](parser: P): ParserMagnet[P] = new ParserMagnet[P](parser)

  implicit def fromCodePointCriterion(criterion: smd.unicode.CodePointCriterion): ParserMagnet[GraphemeParser] =
    GraphemeParser(smd.unicode.GraphemeCriteria.SingleCodePoint(criterion))

  implicit def fromGraphemeCriterion(criterion: smd.unicode.GraphemeCriterion): ParserMagnet[GraphemeParser] =
    GraphemeParser(criterion)

  implicit def fromRegex(regex: scala.util.matching.Regex): ParserMagnet[RegexParser] =
    RegexParser(regex)

  implicit def fromString(str: String): ParserMagnet[LiteralParser] =
    LiteralParser(str)

  implicit def fromUnicodeCategory(category: smd.unicode.UnicodeCategory): ParserMagnet[GraphemeParser] =
    GraphemeParser(smd.unicode.GraphemeCriteria.Category(category))

  implicit def fromUnicodeCategories(categories: scala.collection.Iterable[smd.unicode.UnicodeCategory]): ParserMagnet[GraphemeParser] =
    GraphemeParser(smd.unicode.GraphemeCriteria.Category(categories))
}
