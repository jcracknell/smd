package smd
package grammar

trait ExpressionProductions extends CommonExpressionProductions
                               with LiteralExpressionProductions
                               with IdentifierExpressionProductions
                               with PrimaryExpressionProductions
{
  def Expression: Parser[smd.expressions.Expression] = PrimaryExpression
}
