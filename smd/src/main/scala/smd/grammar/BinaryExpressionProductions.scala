package smd
package grammar

trait BinaryExpressionProductions extends UnaryExpressionProductions {

  lazy val logicalOrExpression = binOp(logicalAndExpression,  "||" ~ !:("=")       ^^^ expression.LogicalOr)

  lazy val logicalAndExpression = binOp(bitwiseOrExpression,  "&&" ~ !:("=")       ^^^ expression.LogicalAnd)

  lazy val bitwiseOrExpression = binOp(bitwiseXOrExpression,  "|"  ~ !:("|" | "=") ^^^ expression.BitwiseOr)

  lazy val bitwiseXOrExpression = binOp(bitwiseAndExpression, "^"  ~ !:("=")       ^^^ expression.BitwiseXOr)

  lazy val bitwiseAndExpression = binOp(equalityExpression,   "&"  ~ !:("&" | "=") ^^^ expression.BitwiseAnd)

  lazy val equalityExpression = binOp(relationalExpression,
    "===" ^^^ expression.StrictEquals
  | "=="  ^^^ expression.Equals
  | "!==" ^^^ expression.StrictNotEquals
  | "!="  ^^^ expression.NotEquals
  )

  lazy val relationalExpression: Parser[Expression] = binOp(shiftExpression,
    ">="         ^^^ expression.GreaterThanOrEqualTo
  | "<="         ^^^ expression.LessThanOrEqualTo
  | ">"          ^^^ expression.GreaterThan
  | "<"          ^^^ expression.LessThan
  | "instanceof" ^^^ expression.InstanceOf
  | "in"         ^^^ expression.In
  )

  lazy val shiftExpression: Parser[Expression] = binOp(additiveExpression,
    "<<"  ^^^ expression.LeftShift
  | ">>>" ^^^ expression.UnsignedRightShift
  | ">>"  ^^^ expression.RightShift
  )

  lazy val additiveExpression: Parser[Expression] = binOp(multiplicativeExpression,
    "+" ^^^ expression.Addition
  | "-" ^^^ expression.Subtraction
  )

  lazy val multiplicativeExpression: Parser[Expression] = binOp(unaryExpression,
    "*" ^^^ expression.Multiplication
  | "/" ^^^ expression.Division
  | "%" ^^^ expression.Modulo
  )

  private def binOp[A](operand: Parser[A], ops: Parser[(A, A) => A]): Parser[A] =
    operand ~ (
      expressionWhitespace ~ ops ~ expressionWhitespace ~ operand ^* { case (_, op, _, rhs) => (op, rhs) }
    ).* ^* { case (lhs, ops) => (lhs /: ops) { (body, op) => op._1(body, op._2) } }
}
