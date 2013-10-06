package smd
package parsing

import scala.collection.mutable.ListBuffer

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

  /** Creates a parser which repeatedly parses the provided parser interleaved with the provided separator parser.
    * Yields a sequence of [[scala.util.Either]] values wherein `rep` results are represented as [[scala.util.Left]]
    * instances, and `sep` results are represented as [[scala.util.Right]].
    *
    * {{{
    * repSep(1, argument, ",") ^* { _.collect { case Left(arg) => arg } }
    * }}}
    *
    * You can merge the products of `rep` and `sep` to their least upper bound:
    *
    * {{{
    * repSep(1, a, b) ^* { _.map(_.merge) }
    * }}}
    *
    * @param n the minimum number of occurrences of the repeated parser.
    * @param rep the repeated parser.
    * @param sep the separator parser.
    * @tparam R the product type of the repeated parser.
    * @tparam S the product type of the separator parser.
    */
  def repSep[R, S](n: Int, rep: Parser[R], sep: Parser[S]): Parser[Seq[Either[R, S]]] = {
    assert(n >= 0, "repSep requires a non-negative number of repetitions")

    if(0 == n)
      repSep(1, rep, sep).? ^* { _.getOrElse(Seq()) }
    else
      rep ~ (sep ~ rep).*>=(n-1) ^~ { (r, srs) =>
        (ListBuffer[Either[R, S]](Left(r)) /: srs) { (lb, sr) => lb += Right(sr._1) += Left(sr._2) }.toList
      }
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
