package smd
package parsing

import org.scalatest.{FunSpec, Matchers}

class ParsingResultSpec extends FunSpec with Matchers {
  describe("Success") {
    describe("range") {
      it("should have correct start and end values") {
        val result = new Success("foo", "foobar", 0, 3)
        (result.range.start) should be (0)
        (result.range.end) should be (2)
      }
    }
  }
  describe("Failure") {
    describe("range") {
      it("should throw an UnsupportedOperationException") {
        intercept[UnsupportedOperationException] { 
          Failure.range
        }
      }
    }
  }
}
