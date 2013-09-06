package smd
package grammar

trait ConditionalExpressionProductions extends BinaryExpressionProductions {
  lazy val ConditionalExpression: Parser[Expression] =
    LogicalOrExpression ~ (
      ExpressionWhitespace ~ "?" ~ ExpressionWhitespace ~
        <>(ConditionalExpression) ~
        ExpressionWhitespace ~ ":" ~ ExpressionWhitespace ~
        <>(ConditionalExpression) >>> { p => (p._4, p._8) }
      ).? >>> {
      case (i, Some((t, e))) => $ex.Conditional(i, t, e)
      case (e, None) => e
    }
}
