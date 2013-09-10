package smd
package grammar

trait LeftHandSideExpressionProductions extends AtExpressionProductions {

  lazy val leftHandSideExpression: Parser[Expression] =
    atExpression ~ (expressionWhitespace ~ (
      // Build a sequence of functions which will construct the appropriate expr when provided a body
      argumentList    ^* { args => (b: Expression) => expression.Call(b, args) } |
      staticProperty  ^* { prop => (b: Expression) => expression.StaticProperty(b, prop) } |
      dynamicProperty ^* { prop => (b: Expression) => expression.DynamicProperty(b, prop) }
    )).* ^* { p =>
      val body = p._1
      val builders: Seq[Expression => Expression] = p._2.map(_._2)
      (body /: builders) { (x, b) => b(x) }
    }

  private lazy val staticProperty = "." ~ expressionWhitespace ~ identifier ^*(_._3)

  private lazy val dynamicProperty = "[" ~ expressionWhitespace ~ <>(expr) ~ expressionWhitespace ~ "]" ^*(_._3)
}
