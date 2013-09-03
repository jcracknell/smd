package smd
package grammar


trait PrimaryExpressionProductions extends LiteralExpressionProductions
                                      with IdentifierExpressionProductions
{
  /** A literal, array literal, or object literal expression. */
  lazy val PrimaryExpression = LiteralExpression | ArrayLiteralExpression | ObjectLiteralExpression

  lazy val ArrayLiteralExpression =
    "[" ~ ExpressionWhitespace ~ ArrayElements ~ ExpressionWhitespace ~ "]" >>> { p => $ex.ArrayLiteral(p._3) }

  private lazy val ArrayElements =
    (
      Expression ~ SubsequentArrayElement.* >>> { p => p._1 +: (p._2.flatten.toSeq) } |
      SubsequentArrayElement.+ >>>(_.flatten) | // initial element elided
      ElidedElements             // all elements elided
    ) ~ ElidedElements >>>(_._1)

  /** A non-elided array element preceded by any number of elided elements. */
  private lazy val SubsequentArrayElement =
    ArgumentSeparator ~ ElidedElements ~ ExpressionWhitespace ~ Expression >>> { p => p._2 :+ p._4 }

  private lazy val ElidedElements = ArgumentSeparator.* >>>(_.map(i => $ex.Elided()))


  lazy val ObjectLiteralExpression =
    "{" ~ ExpressionWhitespace ~ ObjectPropertyAssignments ~ ExpressionWhitespace ~ "}" >>> { p => $ex.ObjectLiteral(p._3) }


  private lazy val ObjectPropertyAssignments =
    (ObjectPropertyAssignment ~ (ArgumentSeparator ~ ObjectPropertyAssignment).* ~ ArgumentSeparator.?) >>> { p => p._1 +: p._2.map(_._2) }

  private lazy val ObjectPropertyAssignment =
    (StringLiteral | NumericLiteral >>>(_.toString) | Identifier) ~
    ExpressionWhitespace ~ ":" ~ ExpressionWhitespace ~ Expression >>> { p => (p._1, p._5) }
}
