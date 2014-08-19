package smd
package runtime

import org.scalatest.{Matchers, FunSpec}

// This spec exists to assert that the double-precision floating point number behavior implemented
// by Scala/Java matches that outlined in the ECMAScript standard which forms the basis for our
// computational model.
class DoubleBehaviorAssumptionsSpec extends FunSpec with Matchers {
  it("NaN != NaN") {
    (Double.NaN != Double.NaN) should be (true)
    (Double.NaN.toString) should be ("NaN")
  }
  it("if either operand is NaN, the result is NaN") {
    ((Double.NaN + 1d).toString) should be ("NaN")
    ((1d + Double.NaN).toString) should be ("NaN")
  }
  it("the sum of two infinities of opposite sign is NaN") {
    ((Double.PositiveInfinity + Double.NegativeInfinity).toString) should be ("NaN")
  }
  it("the sum of two infinities of the same sign is the infinity of that sign") {
    (Double.PositiveInfinity + Double.PositiveInfinity) should be (Double.PositiveInfinity)
    (Double.NegativeInfinity + Double.NegativeInfinity) should be (Double.NegativeInfinity)
  }
  it("the sum of an infinity and a finite value is equal to the infinite operand") {
    (Double.PositiveInfinity + 1d) should be (Double.PositiveInfinity)
    (42d + Double.PositiveInfinity) should be (Double.PositiveInfinity)
    (Double.NegativeInfinity + 1d) should be (Double.NegativeInfinity)
    (42d + Double.NegativeInfinity) should be (Double.NegativeInfinity)
  }
  it("the sum of two negative zeroes is -0") {
    // Here we diverge from spec by not distinguishing between positive and negative zero.
    // Instead we guarantee that a) there is no distinction between 0 and -0, and b) that
    // all values managed by our implementation are positive
    (0d == -0d) should be (true)
    // Assert we can test for -0
    (0d.toString) should be ("0.0")
    ((-0d).toString) should be ("-0.0")
    // Assert we can force a conversion to positive zero
    ((0d + (-0d)).toString) should be ("0.0")
  }
  it("if the magnitude is too large to represent, the operation overflows and the result is then an infinity of appropriate sign") {
    (Double.MaxValue + Double.MaxValue) should be (Double.PositiveInfinity)
    (Double.MinValue - Double.MaxValue) should be (Double.NegativeInfinity)
    // Note that small values are absorbed by the rounding process
    (Double.MaxValue + 1d) should be (Double.MaxValue)
    (Double.MinValue - 1d) should be (Double.MinValue)
  }
}
