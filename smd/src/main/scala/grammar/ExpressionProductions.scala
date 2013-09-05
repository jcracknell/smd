package smd
package grammar

trait ExpressionProductions extends CommonProductions
                               with CommonExpressionProductions
                               with LiteralExpressionProductions
                               with IdentifierExpressionProductions
                               with PrimaryExpressionProductions
                               with AtExpressionProductions
                               with LeftHandSideExpressionProductions
                               with PostfixExpressionProductions
{
  def Expression: Parser[Expression] = PostfixExpression
}
