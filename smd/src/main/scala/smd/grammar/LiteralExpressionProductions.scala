package smd
package grammar

import smd.parsing.{OrderedChoiceParser, Parsers}

trait LiteralExpressionProductions extends Parsers with CommonExpressionProductions {
  lazy val LiteralExpression = NullLiteralExpression |
                               BooleanLiteralExpression |
                               NumericLiteralExpression |
                               StringLiteralExpression

  lazy val NullLiteralExpression = NullLiteral ^^^($ex.NullLiteral())
  lazy val NullLiteral = "null"

  lazy val BooleanLiteralExpression = BooleanLiteral ^* { p => $ex.BooleanLiteral(p) }
  lazy val BooleanLiteral = "true"  ^^^ true |
                            "false" ^^^ false

  lazy val NumericLiteralExpression = NumericLiteral ^* { p => $ex.NumericLiteral(p) }
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

  lazy val StringLiteralExpression = StringLiteral ^* { p => $ex.StringLiteral(p) }

  lazy val StringLiteral = DoubleQuotedStringLiteral | SingleQuotedStringLiteral | VerbatimStringLiteral

  lazy val VerbatimStringLiteral = &:("`") ~ OrderedChoiceParser(
                                     (1 to 16).reverse.map(n => new String(Array.fill(n)('`'))).map { ticks =>
                                       ticks ~> ((!:(ticks) ~ UnicodeCharacter).* ^^(_.parsed)) <~ ticks
                                     }
                                   ) ^*(_._2.toString)

  lazy val DoubleQuotedStringLiteral = "\""  ~> (!:("\"") ~> StringPart).* <~ "\""  ^* { p => new String(p.flatten.toArray) }

  lazy val SingleQuotedStringLiteral = "'"  ~> (!:("'") ~> StringPart).* <~ "'"  ^* { p => new String(p.flatten.toArray) }

  private lazy val StringPart = EscapeSequence ^*(Seq(_)) | !CodePoint.Values('\n', '\r', '\u2028', '\u2029') ^*(_.chars)

  lazy val EscapeSequence = "\\" ~> (CharacterEscape | UnicodeEscapeSequence | HexadecimalEscapeSequence | OctalEscapeSequence)

  private lazy val CharacterEscape = "\'" ^^^ '\'' |
                                     "\"" ^^^ '\"' |
                                     "t"  ^^^ '\t' |
                                     "n"  ^^^ '\n' |
                                     "r"  ^^^ '\r' |
                                     "\\" ^^^ '\\' |
                                     "b"  ^^^ '\b' |
                                     "f"  ^^^ '\f' |
                                     "v"  ^^^ '\u000b'

  private lazy val OctalEscapeSequence =
    CodePoint.Range('0', '7').*(1,3) ^* { p => p.flatMap(_.chars.map(Character.digit(_, 8))).reduce((a, d) => a << 4 | d).toChar }

  private lazy val HexadecimalEscapeSequence =
    "x" ~> HexDigit.*(2) ^* { p => p.flatMap(_.chars.map(Character.digit(_, 16))).reduce((a, d) => a << 4 | d).toChar }
}
