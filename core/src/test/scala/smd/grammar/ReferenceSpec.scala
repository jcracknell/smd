package smd
package grammar

import smd.dom._
import smd.parsing.ParsingScenarios

class ReferenceSpec extends ParsingScenarios {
  import Grammar.reference

  parsing("[google]: http://www.google.com") as reference should produce (Reference(ReferenceId("google"), Seq(IriLiteral("http://www.google.com"))))
  parsing("[google]: (http://www.google.com)") as reference should produce (Reference(ReferenceId("google"), Seq(IriLiteral("http://www.google.com"))))
  parsing(" [google]: http://www.google.com") as reference should produce (Reference(ReferenceId("google"), Seq(IriLiteral("http://www.google.com"))))
  parsing(" [google]: (http://www.google.com)") as reference should produce (Reference(ReferenceId("google"), Seq(IriLiteral("http://www.google.com"))))
  parsing("  [google]: http://www.google.com") as reference should produce (Reference(ReferenceId("google"), Seq(IriLiteral("http://www.google.com"))))
  parsing("  [google]: (http://www.google.com)") as reference should produce (Reference(ReferenceId("google"), Seq(IriLiteral("http://www.google.com"))))
  parsing("   [google]: http://www.google.com") as reference should produce (Reference(ReferenceId("google"), Seq(IriLiteral("http://www.google.com"))))
  parsing("   [google]: (http://www.google.com)") as reference should produce (Reference(ReferenceId("google"), Seq(IriLiteral("http://www.google.com"))))
  // trailing content following block arguments
  parsing("[refid]: url, foo trailing") as reference should reject
  // trailing content following formal arguments
  parsing("[refid]: (url, foo) trailing") as reference should reject
  // space preceding colon
  parsing("[refid] : http://www.google.com") as reference should reject
  // no colon
  parsing("[refid] http://www.google.com") as reference should reject
  // no space following colon
  parsing("[refid]:http://www.google.com") as reference should reject

  parsing("""
  |[my github account]: https://github.com/jcracknell, { 'class' = 'special' }
  """) as reference should produce (
    Reference(ReferenceId("my github account"), Seq(
      IriLiteral("https://github.com/jcracknell"),
      ObjectLiteral("class" -> StringLiteral("special"))
    ))
  )

  parsing("""
  |[my github account]: (https://github.com/jcracknell, { 'class' = 'special' })
  """) as reference should produce (
    Reference(ReferenceId("my github account"), Seq(
      IriLiteral("https://github.com/jcracknell"),
      ObjectLiteral("class" -> StringLiteral("special"))
    ))
  )

  parsing("""
  |[google]: //comment
  |  http://www.google.com
  """) as reference should produce (
    Reference(ReferenceId("google"), List(IriLiteral("http://www.google.com")))
  )

  parsing("""
  |[google]: /* comment
  | comment */http://www.google.com
  """) as reference should produce (
    Reference(ReferenceId("google"), List(IriLiteral("http://www.google.com")))
  )

  parsing("""
  |[google]: http://www.google.com,
  |  "The google home page"
  """) as reference should produce (
    Reference(ReferenceId("google"), List(
      IriLiteral("http://www.google.com"),
      StringLiteral("The google home page")
    ))
  )

  parsing("""
  |[google]: http://www.google.com, //comment
  |  "The google home page"
  """) as reference should produce (
    Reference(ReferenceId("google"), List(
      IriLiteral("http://www.google.com"),
      StringLiteral("The google home page")
    ))
  )

  parsing("""
  |[google]: http://www.google.com, /* comment
  |   comment */ "The google home page"
  """) as reference should produce (
    Reference(ReferenceId("google"), List(
      IriLiteral("http://www.google.com"),
      StringLiteral("The google home page")
    ))
  )
}

