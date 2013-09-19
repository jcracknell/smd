package smd
package markdown

import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers

class ReferenceIdSpec extends FunSpec with ShouldMatchers {
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
