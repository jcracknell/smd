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
    "(" ~ ExpressionWhitespace ~> <>(Expression) <~ ExpressionWhitespace ~ ")"

  lazy val ArrayLiteralExpression =
    "[" ~ ExpressionWhitespace ~ ArrayElements ~ ExpressionWhitespace ~ "]" >>> { p => $ex.ArrayLiteral(p._3) }

  private lazy val ArrayElements =
    (
      <>(Expression) ~ SubsequentArrayElement.* >>> { case (e, ses) => e +: ses.flatten } |
      SubsequentArrayElement.+                  >>> { p => $ex.Elided() +: p.flatten } // initial element elided
    ).? ~ ArgumentSeparator.* >>>(_._1.getOrElse(Seq()))

  /** A non-elided array element preceded by any number of elided elements. */
  private lazy val SubsequentArrayElement =
    ArgumentSeparator.+ ~ ExpressionWhitespace ~ <>(Expression) >>> { case (seps, _, e) => seps.tail.map(_ => $ex.Elided()) :+ e }

  lazy val ObjectLiteralExpression =
    "{" ~ ExpressionWhitespace ~ ObjectPropertyAssignments.? ~ ExpressionWhitespace ~ "}" >>> { case (_,_,ps,_,_) => $ex.ObjectLiteral(ps.getOrElse(Seq())) }

  private lazy val ObjectPropertyAssignments =
    (ObjectPropertyAssignment ~ (ArgumentSeparator ~ ObjectPropertyAssignment).* ~ ArgumentSeparator.?) >>> { p => p._1 +: p._2.map(_._2) }

  private lazy val ObjectPropertyAssignment =
    (StringLiteral | NumericLiteral >>>(_.toString) | Identifier) ~
    ExpressionWhitespace ~ ":" ~ ExpressionWhitespace ~ <>(Expression) >>> { p => (p._1, p._5) }
}
