package smd
package parsing

import org.scalatest.{FunSpec, Matchers}

class ParsingResultSpec extends FunSpec with Matchers {
  describe("Accepted") {
    val success = new Accepted("foo", "foobar", 0, 3)

    it("should have accepted = true") {
      success.accepted should be (true)
    }
    it("should have rejected = false") {
      success.rejected should be (false)
    }
    describe("range") {
      it("should have correct start and end values") {
        (success.range.start) should be (0)
        (success.range.lastElement) should be (2)
      }
    }
  }
  describe("Rejected") {
    it("should have accepted = false") {
      Rejected.accepted should be (false)
    }
    it("should have rejected = true") {
      Rejected.rejected should be (true)
    }
    describe("range") {
      it("should throw an UnsupportedOperationException") {
        intercept[UnsupportedOperationException] {
          Rejected.range
        }
      }
    }
  }
}
