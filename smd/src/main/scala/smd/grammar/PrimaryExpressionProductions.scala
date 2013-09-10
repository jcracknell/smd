package smd
package grammar


trait PrimaryExpressionProductions extends LiteralExpressionProductions
                                      with IdentifierExpressionProductions
{
  /** A literal, array literal, or object literal expression. */
  lazy val primaryExpression: Parser[Expression] =
    literalExpression |
    arrayLiteralExpression |
    objectLiteralExpression |
    "(" ~ expressionWhitespace ~> <>(expr) <~ expressionWhitespace ~ ")"

  lazy val arrayLiteralExpression =
    "[" ~ expressionWhitespace ~ arrayElements ~ expressionWhitespace ~ "]" ^* { p => expression.ArrayLiteral(p._3) }

  private lazy val arrayElements =
    (
      <>(expr) ~ subsequentArrayElement.* ^* { case (e, ses) => e +: ses.flatten } |
      subsequentArrayElement.+                  ^* { p => expression.Elided() +: p.flatten } // initial element elided
    ).? ~ argumentSeparator.* ^*(_._1.getOrElse(Seq()))

  /** A non-elided array element preceded by any number of elided elements. */
  private lazy val subsequentArrayElement =
    argumentSeparator.+ ~ expressionWhitespace ~ <>(expr) ^* { case (seps, _, e) => seps.tail.map(_ => expression.Elided()) :+ e }

  lazy val objectLiteralExpression =
    "{" ~ expressionWhitespace ~ objectPropertyAssignments.? ~ expressionWhitespace ~ "}" ^* { case (_,_,ps,_,_) => expression.ObjectLiteral(ps.getOrElse(Seq())) }

  private lazy val objectPropertyAssignments =
    (objectPropertyAssignment ~ (argumentSeparator ~ objectPropertyAssignment).* ~ argumentSeparator.?) ^* { p => p._1 +: p._2.map(_._2) }

  private lazy val objectPropertyAssignment =
    (stringLiteral | numericLiteral ^*(_.toString) | identifier) ~
    expressionWhitespace ~ ":" ~ expressionWhitespace ~ <>(expr) ^* { p => (p._1, p._5) }
}
