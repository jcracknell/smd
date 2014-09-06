package smd
package parsing

import scala.collection.mutable.ListBuffer

trait Parsers {
  import scala.language.implicitConversions

  protected val CodePoint =       smd.unicode.CodePointCriteria
  protected val Grapheme =        smd.unicode.GraphemeCriteria
  protected val UnicodeCategory = smd.unicode.UnicodeCategory

  /** [[smd.parsing.Parser]] matching the end of the input. */
  protected val EOF = EndOfInputParser

  /** [[smd.parsing.Parser]] matching the empty string, which always accepts and consumes no input. */
  protected val ε = Parser.EmptyString

  /** [[smd.parsing.Parser]] for the empty language, which never accepts and consumes no input. */
  protected val ∅ = Parser.EmptyLanguage

  /** Positive lookahead. Creates an [[smd.parsing.AndPredicateParser]], which consumes no input and succeeds if the
    * provided parser does not succeed.
    *
    * @param parser the parser which must succeed for the lookahead to succeed.
    * @tparam A the product type of the parser.
    */
  protected def ?= [A](parser: Parser[A]): AndPredicateParser[A] = AndPredicateParser(parser)

  /** Negative lookahead. Creates an [[smd.parsing.NotPredicateParser]], which consumes no input and succeeds only if
    * the provided parser does not succeed.
    *
    * @param parser the parser which must fail for the lookahead to succeed.
    * @tparam A the product type of the parser.
    */
  protected def ?! (parser: Parser[Any]): NotPredicateParser = NotPredicateParser(parser)

  /** Creates a lazily initialized [[smd.parsing.ReferenceParser]] from the provided parser.
    *
    * @param parser the parser to be lazily initialized.
    * @tparam A the product type of the lazily initialized parser.
    */
  protected def & [A](parser: => Parser[A]): Parser[A] = ReferenceParser(parser)

  /** Creates an [[smd.parsing.OrderedChoiceParser]] from the provided parsers. */
  protected def |<< [A](parsers: Seq[Parser[A]]): OrderedChoiceParser[A] = OrderedChoiceParser(parsers)

  /** Creates an [[smd.parsing.LongestChoiceParser]] from the provided parsers. */
  protected def |||<< [A](parsers: Seq[Parser[A]]): LongestChoiceParser[A] = LongestChoiceParser(parsers)

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
  protected def repSep[R, S](n: Int, rep: Parser[R], sep: Parser[S]): Parser[List[Either[R, S]]] = {
    assert(n >= 0, "repSep requires a non-negative number of repetitions")

    if(0 == n)
      repSep(1, rep, sep).? ^*^ { _.getOrElse(Nil) }
    else
      rep ~ (sep ~ rep).*>=(n-1) ^*^ { case (r, srs) =>
        (ListBuffer[Either[R, S]](Left(r)) /: srs) { (lb, sr) => lb += Right(sr._1) += Left(sr._2) }.toList
      }
  }

  protected def repSepR[R](n: Int, rep: Parser[R], sep: Parser[Any]): Parser[List[R]] =
    repSep(n, rep, sep) ^*^ { _ collect { case Left(r) => r } }

  protected def repSepS[S](n: Int, rep: Parser[Any], sep: Parser[S]): Parser[List[S]] =
    repSep(n, rep, sep) ^*^ { _ collect { case Right(s) => s } }

  /** For now the primary purpose of this function is to 'seal' the provided parser. */
  protected def rule[A](p: Parser[A]): Parser[A] = p

  /** Adds a method coercing the subject to [[smd.parsing.Parser]] to types for which an implicit conversion exists. */
  implicit class ParserCoercion[A](subject: A) {
    /** Coerces the subject to an instance of [[smd.parsing.Parser]] using implicit conversion. */
    def p[P <: Parser[_]](implicit conv: A => P): P = conv(subject)
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
