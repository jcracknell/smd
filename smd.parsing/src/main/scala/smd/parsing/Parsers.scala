package smd
package parsing

trait Parsers extends ImplicitParserOps {
  import scala.language.implicitConversions

  protected val CodePoint =       smd.unicode.CodePointCriteria
  protected val Grapheme =        smd.unicode.GraphemeCriteria
  protected val UnicodeCategory = smd.unicode.UnicodeCategory

  val EOF = EndOfInputParser

  /** Positive lookahead. Creates an [[smd.parsing.AndPredicateParser]], which consumes no input and succeeds if the
    * provided parser does not succeed.
    *
    * @param parser the parser which must succeed for the lookahead to succeed.
    * @tparam A the product type of the parser.
    */
  def ?= [A](parser: Parser[A]): AndPredicateParser[A] = AndPredicateParser(parser)

  /** Negative lookahead. Creates an [[smd.parsing.NotPredicateParser]], which consumes no input and succeeds only if
    * the provided parser does not succeed.
    *
    * @param parser the parser which must fail for the lookahead to succeed.
    * @tparam A the product type of the parser.
    */
  def ?! (parser: Parser[Any]): NotPredicateParser = NotPredicateParser(parser)

  /** Creates a lazily initialized [[smd.parsing.ReferenceParser]] from the provided parser.
    *
    * @param parser the parser to be lazily initialized.
    * @tparam A the product type of the lazily initialized parser.
    */
  def & [A](parser: => Parser[A]): Parser[A] = ReferenceParser(parser)

  def repSep[A, B](n:Int, rep: Parser[A], sep: Parser[B]): Parser[(Seq[A], Seq[B])] = {
    require(n >= 0, "repSep requires 0 or more repetitions.")
    if(0 == n)
      (rep ~ (sep ~ rep).*).? ^* { o => o.map(p => (p._1 +: p._2.map(_._2), p._2.map(_._1))).getOrElse((Seq(), Seq())) }
    else
      rep ~ (sep ~ rep).*     ^* { p => (p._1 +: p._2.map(_._2), p._2.map(_._1)) }
  }

  implicit def convertCodePointCriterion2Parser(criterion: smd.unicode.CodePointCriterion): GraphemeParser =
    GraphemeParser(Grapheme.SingleCodePoint(criterion))

  implicit def convertGraphemeCriterion2Parser(criterion: smd.unicode.GraphemeCriterion): GraphemeParser =
    GraphemeParser(criterion)

  implicit def convertRegex2Parser(regex: scala.util.matching.Regex): RegexParser = RegexParser(regex)

  implicit def convertString2Parser(str: String): LiteralParser = LiteralParser(str)

  implicit def convertUnicodeCategory2Parser(category: smd.unicode.UnicodeCategory): GraphemeParser =
    GraphemeParser(Grapheme.SingleCodePoint(CodePoint.Category(category)))

  implicit def convertUnicodeCategories2Parser(categories: collection.Iterable[smd.unicode.UnicodeCategory]): GraphemeParser =
    GraphemeParser(Grapheme.SingleCodePoint(CodePoint.Category(categories)))
}
