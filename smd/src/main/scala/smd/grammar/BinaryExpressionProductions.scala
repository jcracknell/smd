package smd
package grammar

trait BinaryExpressionProductions extends UnaryExpressionProductions {

  lazy val LogicalOrExpression = binOp(LogicalAndExpression, "||" ~ !:("=") ^^^ expression.LogicalOr)

  lazy val LogicalAndExpression = binOp(BitwiseOrExpression, "&&" ~ !:("=") ^^^ expression.LogicalAnd)

  lazy val BitwiseOrExpression = binOp(BitwiseXOrExpression, "|" ~ !:("|" | "=") ^^^ expression.BitwiseOr)

  lazy val BitwiseXOrExpression = binOp(BitwiseAndExpression, "^" ~ !:("=") ^^^ expression.BitwiseXOr)

  lazy val BitwiseAndExpression = binOp(EqualityExpression, "&" ~ !:("&" | "=") ^^^ expression.BitwiseAnd)

  lazy val EqualityExpression = binOp(RelationalExpression, EqualityOps)

  private lazy val EqualityOps =
    "===" ^^^ expression.StrictEquals |
    "=="  ^^^ expression.Equals |
    "!==" ^^^ expression.StrictNotEquals |
    "!="  ^^^ expression.NotEquals

  lazy val RelationalExpression: Parser[Expression] = binOp(ShiftExpression, RelationalExpressionOps)

  private lazy val RelationalExpressionOps =
    ">="         ^^^ expression.GreaterThanOrEqualTo |
    "<="         ^^^ expression.LessThanOrEqualTo |
    ">"          ^^^ expression.GreaterThan |
    "<"          ^^^ expression.LessThan |
    "instanceof" ^^^ expression.InstanceOf |
    "in"         ^^^ expression.In

  lazy val ShiftExpression: Parser[Expression] =
    binOp(AdditiveExpression, ("<<" ^^^ expression.LeftShift | ">>>" ^^^ expression.UnsignedRightShift | ">>" ^^^ expression.RightShift))

  lazy val AdditiveExpression: Parser[Expression] =
    binOp(MultiplicativeExpression, ("+" ^^^ expression.Addition | "-" ^^^ expression.Subtraction))

  lazy val MultiplicativeExpression: Parser[Expression] =
    binOp(UnaryExpression, ("*" ^^^ expression.Multiplication | "/" ^^^ expression.Division | "%" ^^^ expression.Modulo))

  private def binOp[A](operand: Parser[A], ops: Parser[(A, A) => A]): Parser[A] =
    operand ~ (
      ExpressionWhitespace ~ ops ~ ExpressionWhitespace ~ operand ^* { case (_, op, _, rhs) => (op, rhs) }
    ).* ^* { case (lhs, ops) => (lhs /: ops) { (body, op) => op._1(body, op._2) } }
}
