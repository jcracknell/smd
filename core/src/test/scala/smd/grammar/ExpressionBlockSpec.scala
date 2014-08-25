package smd
package grammar

import smd.dom._
import smd.parsing.ParsingScenarios

class ExpressionBlockSpec extends ParsingScenarios {
  import Grammar.expressionBlock

  parsing("""
  |@url('foo')
  """) as expressionBlock should produce (
    ExpressionBlock(SourceRange.Unknown, Application(
      Identifier("url"),
      Seq(StringLiteral("foo"))
    ))
  )

  parsing("""
  |// This is a comment
  |   @url('foo')
  """) as expressionBlock should produce (
    ExpressionBlock(SourceRange.Unknown, Application(
      Identifier("url"),
      Seq(StringLiteral("foo"))
    ))
  )

  parsing("""
  | @url('foo'); // comment
  """) as expressionBlock should produce (
    ExpressionBlock(SourceRange.Unknown, Application(
      Identifier("url"),
      Seq(StringLiteral("foo"))
    ))
  )

  parsing("""
  |@author /* comment
  | comment */
  """) as expressionBlock should produce (
    ExpressionBlock(SourceRange.Unknown, Identifier("author"))
  )

  parsing("""@author word""") as expressionBlock should reject
  
  parsing("""word @author""") as expressionBlock should reject

  parsing("""
  |@author
  |word
  """) as expressionBlock should reject
}
