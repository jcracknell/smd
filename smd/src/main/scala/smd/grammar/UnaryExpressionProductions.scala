package smd
package grammar

trait UnaryExpressionProductions extends PostfixExpressionProductions {
  lazy val unaryExpression: Parser[Expression] =
    "!"  ~ expressionWhitespace ~> <>(unaryExpression) ^* expression.LogicalNot      |
    "--" ~ expressionWhitespace ~> <>(unaryExpression) ^* expression.PrefixDecrement |
    "-"  ~ expressionWhitespace ~> <>(unaryExpression) ^* expression.Negative        |
    "++" ~ expressionWhitespace ~> <>(unaryExpression) ^* expression.PrefixIncrement |
    "+"  ~ expressionWhitespace ~> <>(unaryExpression) ^* expression.Positive        |
    "~"  ~ expressionWhitespace ~> <>(unaryExpression) ^* expression.BitwiseNot      |
    "typeof" ~ !:(identifierExpressionPart) ~ expressionWhitespace ~> <>(unaryExpression) ^* expression.Typeof |
    "delete" ~ !:(identifierExpressionPart) ~ expressionWhitespace ~> <>(unaryExpression) ^* expression.Delete |
    "void"   ~ !:(identifierExpressionPart) ~ expressionWhitespace ~> <>(unaryExpression) ^* expression.Void |
    leftHandSideExpression
}
