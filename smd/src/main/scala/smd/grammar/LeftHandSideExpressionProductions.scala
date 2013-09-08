package smd
package grammar

trait LeftHandSideExpressionProductions extends AtExpressionProductions {

  lazy val LeftHandSideExpression: Parser[Expression] =
    AtExpression ~ (ExpressionWhitespace ~ (
      // Build a sequence of functions which will construct the appropriate expr when provided a body
      ArgumentList    ^* { args => (b: Expression) => expression.Call(b, args) } |
      StaticProperty  ^* { prop => (b: Expression) => expression.StaticProperty(b, prop) } |
      DynamicProperty ^* { prop => (b: Expression) => expression.DynamicProperty(b, prop) }
    )).* ^* { p =>
      val body = p._1
      val builders: Seq[Expression => Expression] = p._2.map(_._2)
      (body /: builders) { (x, b) => b(x) }
    }

  private lazy val StaticProperty = "." ~ ExpressionWhitespace ~ Identifier ^*(_._3)

  private lazy val DynamicProperty = "[" ~ ExpressionWhitespace ~ <>(Expr) ~ ExpressionWhitespace ~ "]" ^*(_._3)
}
