package smd
package grammar

trait UnaryExpressionProductions extends PostfixExpressionProductions {
  lazy val UnaryExpression: Parser[Expression] =
    "!"  ~ ExpressionWhitespace ~> <>(UnaryExpression) >>> $ex.LogicalNot      |
    "--" ~ ExpressionWhitespace ~> <>(UnaryExpression) >>> $ex.PrefixDecrement |
    "-"  ~ ExpressionWhitespace ~> <>(UnaryExpression) >>> $ex.Negative        |
    "++" ~ ExpressionWhitespace ~> <>(UnaryExpression) >>> $ex.PrefixIncrement |
    "+"  ~ ExpressionWhitespace ~> <>(UnaryExpression) >>> $ex.Positive        |
    "~"  ~ ExpressionWhitespace ~> <>(UnaryExpression) >>> $ex.BitwiseNot      |
    "typeof" ~ !:(IdentifierExpressionPart) ~ ExpressionWhitespace ~> <>(UnaryExpression) >>>$ex.Typeof |
    "delete" ~ !:(IdentifierExpressionPart) ~ ExpressionWhitespace ~> <>(UnaryExpression) >>>$ex.Delete |
    "void"   ~ !:(IdentifierExpressionPart) ~ ExpressionWhitespace ~> <>(UnaryExpression) >>>$ex.Void |
    LeftHandSideExpression
}
