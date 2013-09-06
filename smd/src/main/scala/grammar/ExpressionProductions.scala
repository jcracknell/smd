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
                               with UnaryExpressionProductions
                               with BinaryExpressionProductions
                               with ConditionalExpressionProductions
{
  lazy val Expression: Parser[Expression] = ConditionalExpression
}
