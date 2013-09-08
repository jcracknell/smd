package smd
package grammar

trait UnaryExpressionProductions extends PostfixExpressionProductions {
  lazy val UnaryExpression: Parser[Expression] =
    "!"  ~ ExpressionWhitespace ~> <>(UnaryExpression) ^* expression.LogicalNot      |
    "--" ~ ExpressionWhitespace ~> <>(UnaryExpression) ^* expression.PrefixDecrement |
    "-"  ~ ExpressionWhitespace ~> <>(UnaryExpression) ^* expression.Negative        |
    "++" ~ ExpressionWhitespace ~> <>(UnaryExpression) ^* expression.PrefixIncrement |
    "+"  ~ ExpressionWhitespace ~> <>(UnaryExpression) ^* expression.Positive        |
    "~"  ~ ExpressionWhitespace ~> <>(UnaryExpression) ^* expression.BitwiseNot      |
    "typeof" ~ !:(IdentifierExpressionPart) ~ ExpressionWhitespace ~> <>(UnaryExpression) ^* expression.Typeof |
    "delete" ~ !:(IdentifierExpressionPart) ~ ExpressionWhitespace ~> <>(UnaryExpression) ^* expression.Delete |
    "void"   ~ !:(IdentifierExpressionPart) ~ ExpressionWhitespace ~> <>(UnaryExpression) ^* expression.Void |
    LeftHandSideExpression
}
