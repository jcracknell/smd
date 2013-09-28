package smd
package grammar

import markdown._
import smd.expression.{StringLiteral, ObjectLiteral, IriLiteral}

class ReferenceSpec extends ProductionSpec {
  def subject = Grammar.reference

  shouldParse("[google]: http://www.google.com") as Reference(ReferenceId("google"), Seq(IriLiteral("http://www.google.com")))
  shouldParse("[google]: (http://www.google.com)") as Reference(ReferenceId("google"), Seq(IriLiteral("http://www.google.com")))
  shouldParse(" [google]: http://www.google.com") as Reference(ReferenceId("google"), Seq(IriLiteral("http://www.google.com")))
  shouldParse(" [google]: (http://www.google.com)") as Reference(ReferenceId("google"), Seq(IriLiteral("http://www.google.com")))
  shouldParse("  [google]: http://www.google.com") as Reference(ReferenceId("google"), Seq(IriLiteral("http://www.google.com")))
  shouldParse("  [google]: (http://www.google.com)") as Reference(ReferenceId("google"), Seq(IriLiteral("http://www.google.com")))
  shouldParse("   [google]: http://www.google.com") as Reference(ReferenceId("google"), Seq(IriLiteral("http://www.google.com")))
  shouldParse("   [google]: (http://www.google.com)") as Reference(ReferenceId("google"), Seq(IriLiteral("http://www.google.com")))

  shouldParse(
    "[my github account]: https://github.com/jcracknell , { 'class': 'special' }"
  ) as (
    Reference(ReferenceId("my github account"), Seq(
      IriLiteral("https://github.com/jcracknell"),
      ObjectLiteral("class" -> StringLiteral("special"))
    ))
  )
  shouldParse(
    "[my github account]: (https://github.com/jcracknell , { 'class': 'special' })"
  ) as (
    Reference(ReferenceId("my github account"), Seq(
      IriLiteral("https://github.com/jcracknell"),
      ObjectLiteral("class" -> StringLiteral("special"))
    ))
  )
}
