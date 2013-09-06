package smd
package grammar

trait BinaryExpressionProductions extends UnaryExpressionProductions {

  lazy val LogicalOrExpression = binOp(LogicalAndExpression, "||" ~ !:("=") >>>> $ex.LogicalOr)

  lazy val LogicalAndExpression = binOp(BitwiseOrExpression, "&&" ~ !:("=") >>>> $ex.LogicalAnd)

  lazy val BitwiseOrExpression = binOp(BitwiseXOrExpression, "|" ~ !:("|" | "=") >>>> $ex.BitwiseOr)

  lazy val BitwiseXOrExpression = binOp(BitwiseAndExpression, "^" ~ !:("=") >>>> $ex.BitwiseXOr)

  lazy val BitwiseAndExpression = binOp(EqualityExpression, "&" ~ !:("&" | "=") >>>> $ex.BitwiseAnd)

  lazy val EqualityExpression = binOp(RelationalExpression, EqualityOps)

  private lazy val EqualityOps =
    "===" >>>> $ex.StrictEquals |
    "=="  >>>> $ex.Equals |
    "!==" >>>> $ex.StrictNotEquals |
    "!="  >>>> $ex.NotEquals

  lazy val RelationalExpression: Parser[Expression] = binOp(ShiftExpression, RelationalExpressionOps)

  private lazy val RelationalExpressionOps =
    ">="         >>>> $ex.GreaterThanOrEqualTo |
    "<="         >>>> $ex.LessThanOrEqualTo |
    ">"          >>>> $ex.GreaterThan |
    "<"          >>>> $ex.LessThan |
    "instanceof" >>>> $ex.InstanceOf |
    "in"         >>>> $ex.In

  lazy val ShiftExpression: Parser[Expression] =
    binOp(AdditiveExpression, ("<<" >>>> $ex.LeftShift | ">>>" >>>> $ex.UnsignedRightShift | ">>" >>>> $ex.RightShift))

  lazy val AdditiveExpression: Parser[Expression] =
    binOp(MultiplicativeExpression, ("+" >>>> $ex.Addition | "-" >>>> $ex.Subtraction))

  lazy val MultiplicativeExpression: Parser[Expression] =
    binOp(UnaryExpression, ("*" >>>> $ex.Multiplication | "/" >>>> $ex.Division | "%" >>>> $ex.Modulo))

  private def binOp[A](operand: Parser[A], ops: Parser[(A, A) => A]): Parser[A] =
    operand ~ (
      ExpressionWhitespace ~ ops ~ ExpressionWhitespace ~ operand >>> { case (_, op, _, rhs) => (op, rhs) }
    ).* >>> { case (lhs, ops) => (lhs /: ops) { (body, op) => op._1(body, op._2) } }
}
