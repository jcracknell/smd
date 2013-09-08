package smd
package grammar

trait PostfixExpressionProductions extends LeftHandSideExpressionProductions {
  lazy val PostfixExpression: Parser[Expression] =
    LeftHandSideExpression ~ ( ExpressionWhitespaceNoNewline ~>
      "--" ^^^ expression.PostfixDecrement |
      "++" ^^^ expression.PostfixIncrement
    ).? ^* {
      case (body, Some(builder)) => builder(body)
      case (body, _) => body
    }
}
