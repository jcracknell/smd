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
    it("should have the correct start and end indices") {
      (success.startIndex) should be (0)
      (success.endIndex) should be (3)
    }
  }
  describe("Rejected") {
    it("should have accepted = false") {
      Rejected.accepted should be (false)
    }
    it("should have rejected = true") {
      Rejected.rejected should be (true)
    }
    it("should throw UnsupportedOperationExceptions") {
      intercept[UnsupportedOperationException] { Rejected.startIndex }
      intercept[UnsupportedOperationException] { Rejected.endIndex }
      intercept[UnsupportedOperationException] { Rejected.length }
      intercept[UnsupportedOperationException] { Rejected.parsed }
    }
  }
}
