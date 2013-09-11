package smd
package grammar

import smd.parsing.{OrderedChoiceParser, Parsers}

trait LiteralExpressionProductions extends Parsers with CommonExpressionProductions {
  lazy val literalExpression = (
    nullLiteralExpression
  | booleanLiteralExpression
  | numericLiteralExpression
  | stringLiteralExpression
  )

  lazy val nullLiteralExpression = nullLiteral ^^^ expression.NullLiteral()
  lazy val nullLiteral = "null"

  lazy val booleanLiteralExpression = booleanLiteral ^* expression.BooleanLiteral

  lazy val booleanLiteral = (
    "true"  ^^^ true
  | "false" ^^^ false
  )

  lazy val numericLiteralExpression = numericLiteral ^* expression.NumericLiteral

  lazy val numericLiteral = {
    val decimalLiteral = {
      val signedInteger =         ("+" ^^^ +1d | "-" ^^^ -1d).? ~ (digit.+ ^^(_.parsed)) ^* { p => p._1.getOrElse(1d) * p._2.toString.toDouble }
      val requiredDecimalPart =   "." ~ digit.+ ^^(_.parsed.toString.toDouble)
      val optionalDecimalPart =   ("." ~ digit.* ^^(_.parsed)).?  ^* { p => p.map(_.toString.toDouble).getOrElse(0d) }
      val optionalExponentPart =  (("e" | "E") ~ signedInteger ^*(_._2)).? ^* { p => math.pow(10d, p.map(_.toString.toDouble).getOrElse(0d)) }
      val decimalIntegerLiteral = ("0" | nonZeroDigit ~ digit.*) ^^(_.parsed.toString.toDouble)

      (
        decimalIntegerLiteral ~ optionalDecimalPart ~ optionalExponentPart ^* { p => (p._1 + p._2) * p._3 }
      | requiredDecimalPart ~ optionalExponentPart                         ^* { p => p._1 * p._2 }
      )
    }

    val hexIntegerLiteral =     "0x" ~ (hexDigit.+ ^^(_.parsed)) ^* { p => java.lang.Long.parseLong(p._2.toString, 16).toDouble }

    decimalLiteral | hexIntegerLiteral
  }


  // String Literals

  lazy val stringLiteralExpression: Parser[expression.StringLiteral] = (
    quotedStringLiteralExpression
  | verbatimStringLiteralExpression
  )

  protected lazy val stringLiteral = (
    quotedStringLiteral
  | verbatimStringLiteral
  )

  lazy val verbatimStringLiteralExpression = verbatimStringLiteral ^* expression.VerbatimStringLiteral

  protected lazy val verbatimStringLiteral = (
    &:("`") ~ OrderedChoiceParser(
      (1 to 16).reverse.map(n => new String(Array.fill(n)('`'))).map { ticks =>
        ticks ~> ((!:(ticks) ~ unicodeCharacter).* ^^(_.parsed)) <~ ticks
      }
    ) ^*(_._2.toString)
  )

  lazy val quotedStringLiteralExpression = quotedStringLiteral ^* expression.QuotedStringLiteral

  protected lazy val quotedStringLiteral = {
    val stringPart = escape ^*(_.flatMap(Character.toChars(_))) | !CodePoint.Values(newLineCharValues) ^*(_.chars)

    OrderedChoiceParser(Seq("\"", "'") map { quot =>
      quot ~> (!:(quot) ~> stringPart).* <~ quot ^* { p => new String(p.flatten.toArray) }
    })
  }
}
