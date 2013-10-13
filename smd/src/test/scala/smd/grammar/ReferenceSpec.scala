package smd
package grammar

import markdown._
import smd.expression.{StringLiteral, ObjectLiteral, IriLiteral}

class ReferenceSpec extends ProductionSpec {
  import Grammar.reference

  parsing("[google]: http://www.google.com") as reference should produce (Reference(ReferenceId("google"), Seq(IriLiteral("http://www.google.com"))))
  parsing("[google]: (http://www.google.com)") as reference should produce (Reference(ReferenceId("google"), Seq(IriLiteral("http://www.google.com"))))
  parsing(" [google]: http://www.google.com") as reference should produce (Reference(ReferenceId("google"), Seq(IriLiteral("http://www.google.com"))))
  parsing(" [google]: (http://www.google.com)") as reference should produce (Reference(ReferenceId("google"), Seq(IriLiteral("http://www.google.com"))))
  parsing("  [google]: http://www.google.com") as reference should produce (Reference(ReferenceId("google"), Seq(IriLiteral("http://www.google.com"))))
  parsing("  [google]: (http://www.google.com)") as reference should produce (Reference(ReferenceId("google"), Seq(IriLiteral("http://www.google.com"))))
  parsing("   [google]: http://www.google.com") as reference should produce (Reference(ReferenceId("google"), Seq(IriLiteral("http://www.google.com"))))
  parsing("   [google]: (http://www.google.com)") as reference should produce (Reference(ReferenceId("google"), Seq(IriLiteral("http://www.google.com"))))

  parsing(
    "[my github account]: https://github.com/jcracknell , { 'class': 'special' }"
  ) as reference should produce (
    Reference(ReferenceId("my github account"), Seq(
      IriLiteral("https://github.com/jcracknell"),
      ObjectLiteral("class" -> StringLiteral("special"))
    ))
  )
  parsing(
    "[my github account]: (https://github.com/jcracknell , { 'class': 'special' })"
  ) as reference should produce (
    Reference(ReferenceId("my github account"), Seq(
      IriLiteral("https://github.com/jcracknell"),
      ObjectLiteral("class" -> StringLiteral("special"))
    ))
  )
}
