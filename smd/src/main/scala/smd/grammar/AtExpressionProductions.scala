package smd
package grammar


trait AtExpressionProductions extends PrimaryExpressionProductions {
  lazy val atExpression: Parser[Expression] = atExpressionRequired | primaryExpression
  lazy val atExpressionRequired: Parser[Expression] = "@" ~ (identifierExpression | primaryExpression) ^*(_._2)
}
