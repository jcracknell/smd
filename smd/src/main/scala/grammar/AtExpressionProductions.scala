package smd
package grammar

import smd.{expressions => expr}

trait AtExpressionProductions extends PrimaryExpressionProductions {
  lazy val AtExpression: Parser[expr.Expression] = AtExpressionRequired | PrimaryExpression
  lazy val AtExpressionRequired: Parser[expr.Expression] = "@" ~ (IdentifierExpression | PrimaryExpression) >>>(_._2)
}
