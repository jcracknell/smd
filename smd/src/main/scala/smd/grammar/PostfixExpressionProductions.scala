package smd
package grammar

trait PostfixExpressionProductions extends LeftHandSideExpressionProductions {
  lazy val postfixExpression: Parser[Expression] =
    leftHandSideExpression ~ ( expressionWhitespaceNoNewline ~>
      "--" ^^^ expression.PostfixDecrement |
      "++" ^^^ expression.PostfixIncrement
    ).? ^* {
      case (body, Some(builder)) => builder(body)
      case (body, _) => body
    }
}
