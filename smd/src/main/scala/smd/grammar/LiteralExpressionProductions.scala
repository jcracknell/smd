package smd
package grammar

import smd.parsing.{OrderedChoiceParser, Parsers}

trait LiteralExpressionProductions extends Parsers with CommonExpressionProductions {
  lazy val LiteralExpression = NullLiteralExpression |
                               BooleanLiteralExpression |
                               NumericLiteralExpression |
                               StringLiteralExpression

  lazy val NullLiteralExpression = NullLiteral ^^^ expression.NullLiteral()
  lazy val NullLiteral = "null"

  lazy val BooleanLiteralExpression = BooleanLiteral ^* expression.BooleanLiteral
  lazy val BooleanLiteral = "true"  ^^^ true |
                            "false" ^^^ false

  lazy val NumericLiteralExpression = NumericLiteral ^* expression.NumericLiteral
  lazy val NumericLiteral = HexIntegerLiteral | DecimalLiteral

  // TODO: Parse hex value
  private lazy val HexIntegerLiteral =     "0x" ~ (HexDigit.+ ^^(_.parsed)) ^* { p => java.lang.Long.parseLong(p._2.toString, 16).toDouble }

  private lazy val DecimalLiteral =
    DecimalIntegerLiteral ~ OptionalDecimalPart ~ OptionalExponentPart ^* { p => (p._1 + p._2) * p._3 } |
    RequiredDecimalPart ~ OptionalExponentPart                         ^* { p => p._1 * p._2 }

  private lazy val RequiredDecimalPart =   "." ~ Digit.+ ^^(_.parsed.toString.toDouble)
  private lazy val OptionalDecimalPart =   ("." ~ Digit.* ^^(_.parsed)).?  ^* { p => p.map(_.toString.toDouble).getOrElse(0d) }
  private lazy val OptionalExponentPart =  (("e" | "E") ~ SignedInteger ^*(_._2)).? ^* { p => math.pow(10d, p.map(_.toString.toDouble).getOrElse(0d)) }
  private lazy val SignedInteger =         ("+" ^^^ +1d | "-" ^^^ -1d).? ~ (Digit.+ ^^(_.parsed)) ^* { p => p._1.getOrElse(1d) * p._2.toString.toDouble }
  private lazy val DecimalIntegerLiteral = ("0" | NonZeroDigit ~ Digit.*) ^^(_.parsed.toString.toDouble)


  // String Literals

  lazy val StringLiteralExpression: Parser[expression.StringLiteral] = QuotedStringLiteralExpression | VerbatimStringLiteralExpression

  lazy val StringLiteral = QuotedStringLiteral | VerbatimStringLiteral

  lazy val VerbatimStringLiteralExpression = VerbatimStringLiteral ^* expression.VerbatimStringLiteral

  lazy val VerbatimStringLiteral = &:("`") ~ OrderedChoiceParser(
                                     (1 to 16).reverse.map(n => new String(Array.fill(n)('`'))).map { ticks =>
                                       ticks ~> ((!:(ticks) ~ UnicodeCharacter).* ^^(_.parsed)) <~ ticks
                                     }
                                   ) ^*(_._2.toString)

  lazy val QuotedStringLiteralExpression = QuotedStringLiteral ^* expression.QuotedStringLiteral

  lazy val QuotedStringLiteral = DoubleQuotedStringLiteral | SingleQuotedStringLiteral

  lazy val DoubleQuotedStringLiteral = "\""  ~> (!:("\"") ~> StringPart).* <~ "\""  ^* { p => new String(p.flatten.toArray) }

  lazy val SingleQuotedStringLiteral = "'"  ~> (!:("'") ~> StringPart).* <~ "'"  ^* { p => new String(p.flatten.toArray) }

  private lazy val StringPart = Escape ^*(_.flatMap(Character.toChars(_))) | !CodePoint.Values(NewLineCharValues) ^*(_.chars)
}
