package smd
package dom

import org.scalatest.{FunSpec, Matchers}

class ReferenceIdSpec extends FunSpec with Matchers {
  describe("normalization") {
    Map(
      "A"         -> "a",
      "1"         -> "1",
      ""          -> "",
      "  foo  "   -> "foo",
      "FOO bAr"   -> "foo-bar",
      "foo  bar"  -> "foo-bar",
      "foo - bar" -> "foo-bar",
      "resumé"    -> "resume"
    ) foreach { case (raw, expected) =>
      it(s"${raw.literalEncode} -> ${expected.literalEncode}") {
        ReferenceId.normalize(raw) should be (expected)
      }
    }
  }
}
