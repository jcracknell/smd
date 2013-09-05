package smd
package grammar

trait PostfixExpressionProductions extends LeftHandSideExpressionProductions {
  lazy val PostfixExpression: Parser[Expression] =
    LeftHandSideExpression ~ ( ExpressionWhitespaceNoNewline ~>
      "--" >>>>((body: Expression) => $ex.PostfixDecrement(body)) |
      "++" >>>>((body: Expression) => $ex.PostfixIncrement(body))
    ).? >>> {
      case (body, Some(builder)) => builder(body)
      case (body, _) => body
    }
}
