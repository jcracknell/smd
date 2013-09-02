package smd
package grammar

trait ExpressionProductions extends CommonProductions
                               with CommonExpressionProductions
                               with LiteralExpressionProductions
                               with IdentifierExpressionProductions
                               with PrimaryExpressionProductions
                               with AtExpressionProductions
                               with LeftHandSideExpressionProductions
{
  def Expression: Parser[smd.expressions.Expression] = LeftHandSideExpression
}
