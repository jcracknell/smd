package smd
package unicode

import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers

class CodePointInfoSpec extends FunSpec with ShouldMatchers {
  describe("at method") {
    it("should work for first plane character") {
      CodePointInfo.at("a", 0) shouldEqual CodePointInfo(0x61, Character.LOWERCASE_LETTER, 1)
    }
    it("should work for an invalid surrogate") {
      CodePointInfo.at("\u8812a", 0) shouldEqual CodePointInfo(0x8812, Character.SURROGATE, 1)
    }
    it("should word for the minimum surrogate pair") {
      CodePointInfo.at("\uD800\uDC00", 0) shouldEqual CodePointInfo(0x10000, Character.OTHER_LETTER, 2)
    }
    it("should work for the maximum surrogate pair") {
      CodePointInfo.at("\uDBFF\uDFFF", 0) shouldEqual CodePointInfo(0x10FFFF, Character.OTHER_LETTER, 2)
    }
  }
}
