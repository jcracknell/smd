package smd
package util

import org.scalatest.{Matchers, FunSpec}

class HexEncodingSpec extends FunSpec with Matchers {
  describe("encodeDigitLower") {
    it("should encode correctly") {
      (Array(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)).map(HexEncoding.encodeDigitLower) should be ("0123456789abcdef".toCharArray)
    }
  }
  describe("encodeDigitUpper") {
    it("should encode correctly") {
      (Array(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)).map(HexEncoding.encodeDigitUpper) should be ("0123456789ABCDEF".toCharArray)
    }
  }
  describe("decodeDigit") {
    it("should decode the correct values for all valid hexadecimal digits") {
      Map(
        '0' ->  0, '1' ->  1, '2' ->  2, '3' ->  3, '4' ->  4, '5' ->  5, '6' ->  6, '7' ->  7, '8' ->  8,
        '9' ->  9, 'A' -> 10, 'a' -> 10, 'B' -> 11, 'b' -> 11, 'C' -> 12, 'c' -> 12, 'D' -> 13, 'd' -> 13,
        'E' -> 14, 'e' -> 14, 'F' -> 15, 'f' -> 15
      ) foreach { case (c, v) => HexEncoding.decodeDigit(c) should be (Some(v)) }
    }
  }
  describe("encodeLower(value:Byte):String") {
    it("should encode correctly") {
      (HexEncoding.encodeLower(0xEF.toByte)) should be ("ef")
    }
  }
  describe("encodeUpper(value:Byte):String") {
    it("should encode correctly") {
     (HexEncoding.encodeUpper(0xEF.toByte)) should be ("EF")
    }
  }
  describe("encodeLower(value:Short):String") {
    it("should encode correctly") {
      (HexEncoding.encodeLower(0xCDEF.toShort)) should be ("cdef")
    }
  }
  describe("encodeUpper(value:Short):String") {
    it("should encode correctly") {
      (HexEncoding.encodeUpper(0xCDEF.toShort)) should be ("CDEF")
    }
  }
  describe("encodeLower(value:Int):String") {
    it("should encode correctly") {
      (HexEncoding.encodeLower(0x89ABCDEF)) should be ("89abcdef")
    }
  }
  describe("encodeUpper(value:Int):String") {
    it("should encode correctly") {
      (HexEncoding.encodeUpper(0x89ABCDEF)) should be ("89ABCDEF")
    }
  }
  describe("encodeLower(value:Long):String") {
    it("should encode correctly") {
      (HexEncoding.encodeLower(0x0123456789ABCDEFL)) should be ("0123456789abcdef")
    }
  }
  describe("encodeUpper(value:Long):String") {
    it("should encode correctly") {
      (HexEncoding.encodeUpper(0x0123456789ABCDEFL)) should be ("0123456789ABCDEF")
    }
  }
}
