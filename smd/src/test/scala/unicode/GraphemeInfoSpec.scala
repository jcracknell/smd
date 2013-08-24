package smd
package unicode

import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers

class GraphemeInfoSpec extends FunSpec with ShouldMatchers {
  describe("at method") {
    it("should work for \"a\"") {
      GraphemeInfo.at("a", 0) shouldEqual GraphemeInfo("a", 0, 1, Character.LOWERCASE_LETTER)
    }
    it("should work for \"ab\"") {
      GraphemeInfo.at("ab", 0) shouldEqual GraphemeInfo("ab", 0, 1, Character.LOWERCASE_LETTER)
    }
    it("should work for 'a' w/ combining ring") {
      GraphemeInfo.at("a\u030a", 0) shouldEqual GraphemeInfo("a\u030a", 0, 2, Character.LOWERCASE_LETTER)
    }
    it("should work for 'a' w/ combining ring & caron") {
      GraphemeInfo.at("a\u030a\u030c", 0) shouldEqual GraphemeInfo("a\u030a\u030c", 0, 3, Character.LOWERCASE_LETTER)
    }
    it("should work for combining ring & 'a' w/ combining ring") {
      GraphemeInfo.at("\u030aa\u030a", 0) shouldEqual GraphemeInfo("\u030aa\u030a", 0, 1, Character.NON_SPACING_MARK)
    }
  }
}
