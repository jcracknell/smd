package smd
package grammar

import smd.{expressions => expr}

trait LeftHandSideExpressionProductions extends AtExpressionProductions {

  lazy val LeftHandSideExpression =
    AtExpression ~ (ExpressionWhitespace ~ (
      CallExpression | StaticProperty | DynamicProperty
    )).* >>> { p =>
      val base = p._1
      val builders: Seq[expr.Expression => expr.Expression] = p._2.map(_._2)
      (base /: builders) { (x, b) => b(x) }
    }

  private lazy val CallExpression = ArgumentList >>>{ p => mkBuilder(body => expr.CallExpression(body, p)) }

  private lazy val StaticProperty =
    "." ~ ExpressionWhitespace ~ Identifier >>>{ p => mkBuilder(body => expr.StaticPropertyExpression(body, p._3)) }

  private lazy val DynamicProperty =
    "[" ~ ExpressionWhitespace ~
    Expression ~
    ExpressionWhitespace ~ "]" >>>{ p => mkBuilder(body => expr.DynamicPropertyExpression(body, p._3)) }

  private def mkBuilder(build: expr.Expression => expr.Expression): (expr.Expression => expr.Expression) = build

  lazy val ArgumentList = "(" ~ ExpressionWhitespace ~ ArgumentListArguments ~ ExpressionWhitespace ~ ")" >>>(_._3)

  private lazy val ArgumentListArguments = Expression ~ (ArgumentSeparator ~ Expression).* >>> { p => p._1 +: p._2.map(_._2) }
}
