package smd
package grammar

trait ConditionalExpressionProductions extends BinaryExpressionProductions {
  lazy val conditionalExpression: Parser[Expression] = (
    logicalOrExpression
  ~ ( expressionWhitespace ~ "?" ~ expressionWhitespace
    ~ <>(conditionalExpression)
    ~ expressionWhitespace ~ ":" ~ expressionWhitespace
    ~ <>(conditionalExpression)
    ^*{ p => (p._4, p._8) }
    ).? ^* {
      case (i, Some((t, e))) => expression.Conditional(i, t, e)
      case (e, None) => e
    }
  )

}
