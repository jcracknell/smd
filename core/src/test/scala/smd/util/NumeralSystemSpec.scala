package smd
package util

import org.scalatest.{FunSpec, Matchers}

class NumeralSystemSpec extends FunSpec with Matchers {
  import NumeralSystem._

  describe("Alpha") {
    it("should decode 'a'") {
      Alpha.decode("a") should be (Some(1))
    }
    it("should decode 'A'") {
      Alpha.decode("A") should be (Some(1))
    }
    it("should decode 'z'") {
      Alpha.decode("z") should be (Some(26))
    }
    it("should decode 'Z'") {
      Alpha.decode("Z") should be (Some(26))
    }
    it("should decode 'aa'") {
      Alpha.decode("aa") should be (Some(27))
    }
    it("should decode 'ab'") {
      Alpha.decode("ab") should be (Some(28))
    }
    it("should encode 1") {
      Alpha.encode(1) should be (Some("a"))
    }
    it("should encode 26") {
      Alpha.encode(26) should be (Some("z"))
    }
    it("should not encode 0") {
      Alpha.encode(0) should be (None)
    }
    it("should not encode -1") {
      Alpha.encode(-1) should be (None)
    }
    it("should encode 27") {
      Alpha.encode(27) should be (Some("aa"))
    }
    it("should encode 28") {
      Alpha.encode(28) should be (Some("ab"))
    }
    it("should encode Int.MaxValue") {
      Alpha.encode(Int.MaxValue) should be (Some("fxshrxw"))
    }
  }
}
