package smd
package dom

import org.scalatest.{FunSpec, Matchers}

class SourceRangeSpec extends FunSpec with Matchers {
  describe("equality") {
    it("should work as expected for two values") {
      (SourceRange.Value(0, 0) == SourceRange.Value(0, 0)) should be (true)
      (SourceRange.Value(1, 2) == SourceRange.Value(1, 2)) should be (true)
      (SourceRange.Value(1, 2) == SourceRange.Value(1, 3)) should be (false)
      (SourceRange.Value(0, 2) == SourceRange.Value(1, 2)) should be (false)
    }
    it("should work as expected for two unknowns") {
      (SourceRange.Unknown == SourceRange.Unknown) should be (true)
    }
    it("should work as expected for an unknown and a value") {
      (SourceRange.Value(1, 2) == SourceRange.Unknown) should be (true)
      (SourceRange.Unknown == SourceRange.Value(1, 2)) should be (true)
    }
  }
}
