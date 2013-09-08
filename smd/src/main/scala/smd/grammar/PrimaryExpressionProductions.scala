package smd
package grammar


trait PrimaryExpressionProductions extends LiteralExpressionProductions
                                      with IdentifierExpressionProductions
{
  /** A literal, array literal, or object literal expression. */
  lazy val PrimaryExpression: Parser[Expression] =
    LiteralExpression |
    ArrayLiteralExpression |
    ObjectLiteralExpression |
    "(" ~ ExpressionWhitespace ~> <>(Expr) <~ ExpressionWhitespace ~ ")"

  lazy val ArrayLiteralExpression =
    "[" ~ ExpressionWhitespace ~ ArrayElements ~ ExpressionWhitespace ~ "]" ^* { p => expression.ArrayLiteral(p._3) }

  private lazy val ArrayElements =
    (
      <>(Expr) ~ SubsequentArrayElement.* ^* { case (e, ses) => e +: ses.flatten } |
      SubsequentArrayElement.+                  ^* { p => expression.Elided() +: p.flatten } // initial element elided
    ).? ~ ArgumentSeparator.* ^*(_._1.getOrElse(Seq()))

  /** A non-elided array element preceded by any number of elided elements. */
  private lazy val SubsequentArrayElement =
    ArgumentSeparator.+ ~ ExpressionWhitespace ~ <>(Expr) ^* { case (seps, _, e) => seps.tail.map(_ => expression.Elided()) :+ e }

  lazy val ObjectLiteralExpression =
    "{" ~ ExpressionWhitespace ~ ObjectPropertyAssignments.? ~ ExpressionWhitespace ~ "}" ^* { case (_,_,ps,_,_) => expression.ObjectLiteral(ps.getOrElse(Seq())) }

  private lazy val ObjectPropertyAssignments =
    (ObjectPropertyAssignment ~ (ArgumentSeparator ~ ObjectPropertyAssignment).* ~ ArgumentSeparator.?) ^* { p => p._1 +: p._2.map(_._2) }

  private lazy val ObjectPropertyAssignment =
    (StringLiteral | NumericLiteral ^*(_.toString) | Identifier) ~
    ExpressionWhitespace ~ ":" ~ ExpressionWhitespace ~ <>(Expr) ^* { p => (p._1, p._5) }
}
