package smd
package grammar

import smd.{expressions => expr}

trait LeftHandSideExpressionProductions extends AtExpressionProductions {

  lazy val LeftHandSideExpression: Parser[expr.Expression] =
    AtExpression ~ (ExpressionWhitespace ~ (
      // Build a sequence of functions which will construct the appropriate expr when provided a body
      ArgumentList    >>> { args => (b: expr.Expression) => expr.CallExpression(b, args) } |
      StaticProperty  >>> { prop => (b: expr.Expression) => expr.StaticPropertyExpression(b, prop) } |
      DynamicProperty >>> { prop => (b: expr.Expression) => expr.DynamicPropertyExpression(b, prop) }
    )).* >>> { p =>
      val body = p._1
      val builders: Seq[expr.Expression => expr.Expression] = p._2.map(_._2)
      (body /: builders) { (x, b) => b(x) }
    }

  private lazy val StaticProperty = "." ~ ExpressionWhitespace ~ Identifier >>>(_._3)

  private lazy val DynamicProperty = "[" ~ ExpressionWhitespace ~ Expression ~ ExpressionWhitespace ~ "]" >>>(_._3)
}
