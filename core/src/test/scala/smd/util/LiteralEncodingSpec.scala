package smd
package util

import org.scalatest.{Matchers, FunSpec}

class LiteralEncodingSpec extends FunSpec with Matchers {
  describe("encode(c:Char):String") {
    def resultOfApplyingTo(c:Char) = LiteralEncoding.encode(c)

    it("should escape characters requiring escape sequences") {
      resultOfApplyingTo('\b') should be ("'\\b'")
      resultOfApplyingTo('\t') should be ("'\\t'")
      resultOfApplyingTo('\n') should be ("'\\n'")
      resultOfApplyingTo('\f') should be ("'\\f'")
      resultOfApplyingTo('\r') should be ("'\\r'")
      resultOfApplyingTo('\\') should be ("'\\\\'")
      resultOfApplyingTo('\u0000') should be ("'\\u0000'")
      resultOfApplyingTo('\'') should be ("'\\''")
    }
    it("should not escape double quotes") {
      resultOfApplyingTo('"') should be ("'\"'")
    }
    it("should escape unicode characters") {
      resultOfApplyingTo('ɷ') should be ("'\\u0277'")
    }
  }
  describe("encode(str:String):String") {
    def resultOfApplyingTo(str: String) = LiteralEncoding.encode(str)

    it("should escape characters requiring escape sequences") {
      resultOfApplyingTo("\b\t\n\f\r\"\\\u0000") should be ("\"\\b\\t\\n\\f\\r\\\"\\\\\\u0000\"")
    }
    it("should not escape single quotes") {
      resultOfApplyingTo("'") should be ("\"'\"")
    }
    it("should not escape regular printable ascii characters") {
      resultOfApplyingTo("foo") should be ("\"foo\"")
    }
    it("should escape upper-ascii characters") {
      resultOfApplyingTo("æ") should be ("\"\\u00e6\"")
    }
    it("should escape unicode characters") {
      resultOfApplyingTo("ɷ") should be ("\"\\u0277\"")
    }
    it("should encode null values") {
      resultOfApplyingTo(null) should be ("null")
    }
  }
}
