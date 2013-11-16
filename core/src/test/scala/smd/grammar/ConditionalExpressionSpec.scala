package smd
package grammar

import smd.parsing.ParsingScenarios
import smd.dom._

class ConditionalExpressionSpec extends ParsingScenarios {
  import Grammar.conditionalExpression

  parsing("if 1 then 2 else 3") as conditionalExpression should produce (
    Conditional(NumericLiteral(1d), NumericLiteral(2d), Some(NumericLiteral(3d)))
  )

  parsing("if(1) 2 else 3") as conditionalExpression should produce (
    Conditional(NumericLiteral(1d), NumericLiteral(2d), Some(NumericLiteral(3d)))
  )

  parsing("if+1 then-2 else+3") as conditionalExpression should produce (
    Conditional(Positive(NumericLiteral(1d)), Negative(NumericLiteral(2d)), Some(Positive(NumericLiteral(3d))))
  )

  // This does not currently work because the rest of the expr is globbed up as an iri literal
  /*
  parsing("if+1then-2else+3") as conditionalExpression should produce (
    Conditional(Positive(NumericLiteral(1d)), Negative(NumericLiteral(2d)), Some(Positive(NumericLiteral(3d))))
  )
  */

  parsing("if (1) 2 else 3") as conditionalExpression should produce (
    Conditional(NumericLiteral(1d), NumericLiteral(2d), Some(NumericLiteral(3d)))
  )

  parsing("if(1)2 else 3") as conditionalExpression should produce (
    Conditional(NumericLiteral(1d), NumericLiteral(2d), Some(NumericLiteral(3d)))
  )

  parsing("if 1 then 2") as conditionalExpression should produce (
    Conditional(NumericLiteral(1d), NumericLiteral(2d))
  )

  parsing("if(1) 2") as conditionalExpression should produce (
    Conditional(NumericLiteral(1d), NumericLiteral(2d))
  )
}
