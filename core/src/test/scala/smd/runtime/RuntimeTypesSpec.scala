package smd
package runtime

import org.scalatest.{FunSpec, Matchers}

class RuntimeTypesSpec extends FunSpec with Matchers with RuntimeTypes {
  describe("XNumber") {
    describe("factory") {
      it("should handle the general case") {
        (XNumber(1d).value) should === (1d)
        (XNumber(0d).value) should === (0d)
        (XNumber(-1d).value) should === (-1d)
      }
      it("should handle NaN") {
        (XNumber(Double.NaN)) should be (XNumber.NaN)
      }
      it("should handle positive infinity") {
        (XNumber(Double.PositiveInfinity)) should be (XNumber.PosInf)
      }
      it("should handle negative infinity") {
        (XNumber(Double.NegativeInfinity)) should be (XNumber.NegInf)
      }
      it("should handle negative zero") {
        (XNumber(-0d).value.toString) should be ("-0.0")
      }
    }
  }
}
