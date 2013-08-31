package smd
package grammar

import smd.{expressions => expr}
import smd.parsing.{OrderedChoiceParser, Parsers}

trait ExpressionProductions extends Parsers with CommonProductions {
  lazy val LiteralExpression = NullLiteralExpression |
                               BooleanLiteralExpression |
                               NumericLiteralExpression |
                               StringLiteralExpression

  lazy val NullLiteralExpression = NullLiteral >>>>(expr.NullLiteralExpression)
  lazy val NullLiteral = "null"

  lazy val BooleanLiteralExpression = BooleanLiteral >>> { p => expr.BooleanLiteralExpression(p) }
  lazy val BooleanLiteral = "true"  >>>> true |
                            "false" >>>> false

  lazy val NumericLiteralExpression = NumericLiteral >>> { p => expr.NumericLiteralExpression(p) }
  lazy val NumericLiteral = HexIntegerLiteral | DecimalLiteral

  // TODO: Parse hex value
  private lazy val HexIntegerLiteral =     "0x" ~ (HexDigit.+ >>(_.parsed)) >>>(_._2.toString.toDouble)

  private lazy val DecimalLiteral =
    DecimalIntegerLiteral ~ OptionalDecimalPart ~ OptionalExponentPart >>> { p => (p._1 + p._2) * p._3 } |
    RequiredDecimalPart ~ OptionalExponentPart                         >>> { p => p._1 * p._2 }

  private lazy val RequiredDecimalPart =   "." ~ Digit.+ >>(_.parsed.toString.toDouble)
  private lazy val OptionalDecimalPart =   ("." ~ Digit.* >>(_.parsed)).?  >>> { p => p.map(_.toString.toDouble).getOrElse(0d) }
  private lazy val OptionalExponentPart =  (("e" | "E") ~ SignedInteger >>>(_._2)).? >>> { p => math.pow(10d, p.map(_.toString.toDouble).getOrElse(0d)) }
  private lazy val SignedInteger =         ("+" | "-").? ~ Digit.+        >>(_.parsed.toString.toDouble)
  private lazy val DecimalIntegerLiteral = ("0" | NonZeroDigit ~ Digit.*) >>(_.parsed.toString.toDouble)


  // String Literals

  lazy val StringLiteralExpression = StringLiteral >>> { p => expr.StringLiteralExpression(p.toString) }

  lazy val StringLiteral = DoubleQuotedStringLiteral | SingleQuotedStringLiteral | VerbatimStringLiteral

  lazy val VerbatimStringLiteral = &:("`") ~ OrderedChoiceParser(
                                     (1 to 16).reverse.map(n => new String(Array.fill(n)('`'))).map { ticks =>
                                       ticks ~ ((!:(ticks) ~ UnicodeCharacter).* >>(_.parsed)) ~ ticks >>>(_._2)
                                     }
                                   ) >>>(_._2)

  lazy val DoubleQuotedStringLiteral = "\"" ~ ((!:("\"") ~ StringLiteralCharacter).* >>(_.parsed)) ~ "\"" >>>(_._2)

  lazy val SingleQuotedStringLiteral = "'"  ~ ((!:("'")  ~ StringLiteralCharacter).* >>(_.parsed)) ~ "'"  >>>(_._2)

  lazy val StringLiteralCharacter = ("\\" ~ (UnicodeEscapeSequence | HexadecimalEscapeSequence | NewLine | UnicodeCharacter)) |
                                    !CodePoint.Values('\n', '\r', '\u2028', '\u2029')

  lazy val Keyword = "break" | "case" | "catch" | "class" | "const" | "continue" | "debugger" |
                     "default" | "delete" | "do" | "else" | "enum" | "export" | "extends" |
                     "false" | "finally" | "for" | "function" | "if" | "import" | "instanceof" |
                     "in" | "new" | "null" | "return" | "super" | "switch" | "this" | "throw" |
                     "true" | "try" | "typeof" | "var" | "void" | "while" | "with"

  /** Zero or more space characters or comments. */
  lazy val ExpressionWhitespaceNoNewline = (SpaceChar | Comment).*

  /** Zero or more whitespace characters or comments. */
  lazy val ExpressionWhitespace = (Whitespace | Comment).*

  lazy val HexadecimalEscapeSequence = "x" ~ HexDigit.*(2)
  lazy val UnicodeEscapeSequence =     "u" ~ HexDigit.*(4)
}
