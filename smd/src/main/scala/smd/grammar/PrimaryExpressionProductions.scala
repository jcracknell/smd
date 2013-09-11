package smd
package grammar


trait PrimaryExpressionProductions extends LiteralExpressionProductions
                                      with IdentifierExpressionProductions
{
  /** A literal, array literal, or object literal expression. */
  lazy val primaryExpression: Parser[Expression] = (
    literalExpression
  | arrayLiteralExpression
  | objectLiteralExpression
  | "(" ~ expressionWhitespace ~> <>(expr) <~ expressionWhitespace ~ ")"
  )

  lazy val arrayLiteralExpression: Parser[expression.ArrayLiteral] = {
    /** A non-elided array element preceded by any number of elided elements. */
    val subsequentArrayElement =
      argumentSeparator.+ ~ expressionWhitespace ~ <>(expr) ^* { case (seps, _, e) => seps.tail.map(_ => expression.Elided()) :+ e }

    val arrayElements = (
      (
        <>(expr) ~ subsequentArrayElement.* ^* { case (e, ses) => e +: ses.flatten }
      | subsequentArrayElement.+            ^* { p => expression.Elided() +: p.flatten } // initial element elided
      ).?
    <~ argumentSeparator.*
    ^*(_.getOrElse(Seq()))
    )

    "[" ~ expressionWhitespace ~> arrayElements <~ expressionWhitespace ~ "]" ^* expression.ArrayLiteral
  }

  lazy val objectLiteralExpression: Parser[expression.ObjectLiteral] = {
    val objectPropertyAssignment = (
      (stringLiteral | numericLiteral ^*(_.toString) | identifier)
    ~ expressionWhitespace ~ ":" ~ expressionWhitespace
    ~ <>(expr)
    ^* { p => (p._1, p._5) }
    )

    val objectPropertyAssignments = (
      objectPropertyAssignment
    ~ (argumentSeparator ~ objectPropertyAssignment).*
    ~ argumentSeparator.?
    ^* { p => p._1 +: p._2.map(_._2) }
    )

    (
      "{" ~ expressionWhitespace ~> objectPropertyAssignments.? <~ expressionWhitespace ~ "}"
    ^*{ ps => expression.ObjectLiteral(ps.getOrElse(Seq())) }
    )
  }
}
