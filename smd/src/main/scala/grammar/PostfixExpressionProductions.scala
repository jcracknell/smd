package smd
package grammar

trait PostfixExpressionProductions extends LeftHandSideExpressionProductions {
  lazy val PostfixExpression: Parser[Expression] =
    LeftHandSideExpression ~ ( ExpressionWhitespaceNoNewline ~>
      "--" >>>> $ex.PostfixDecrement |
      "++" >>>> $ex.PostfixIncrement
    ).? >>> {
      case (body, Some(builder)) => builder(body)
      case (body, _) => body
    }
}
