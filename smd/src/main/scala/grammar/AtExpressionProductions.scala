package smd
package grammar


trait AtExpressionProductions extends PrimaryExpressionProductions {
  lazy val AtExpression: Parser[Expression] = AtExpressionRequired | PrimaryExpression
  lazy val AtExpressionRequired: Parser[Expression] = "@" ~ (IdentifierExpression | PrimaryExpression) >>>(_._2)
}
