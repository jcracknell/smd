package smd
package util

import org.scalatest.{FunSpec, Matchers}

class Base64EncodingSpec extends FunSpec with Matchers {
  def utf8Bytes(str: String*): Array[Byte] =
    str.mkString("").getBytes(java.nio.charset.Charset.forName("UTF-8"))

  it("should encode 0 characters") {
    Base64Encoding.encode(utf8Bytes("")) should be ("")
  }

  it("should encode 1 characters") {
    Base64Encoding.encode(utf8Bytes("c")) should be ("Yw==")
  }

  it("should encode 2 characters") {
    Base64Encoding.encode(utf8Bytes("ca")) should be ("Y2E=")
  }

  it("should encode 3 characters") {
    Base64Encoding.encode(utf8Bytes("cat")) should be ("Y2F0")
  }

  it("should encode 4 characters") {
    Base64Encoding.encode(utf8Bytes("cats")) should be ("Y2F0cw==")
  }

  it("should encode Thomas Hobbes' 'Leviathan' per Wikipedia") {
    Base64Encoding.encode(utf8Bytes(
      "Man is distinguished, not only by his reason, but by this singular passion from " +
      "other animals, which is a lust of the mind, that by a perseverance of delight " +
      "in the continued and indefatigable generation of knowledge, exceeds the short " +
      "vehemence of any carnal pleasure."
    )) should be (
      "TWFuIGlzIGRpc3Rpbmd1aXNoZWQsIG5vdCBvbmx5IGJ5IGhpcyByZWFzb24sIGJ1dCBieSB0aGlz" +
      "IHNpbmd1bGFyIHBhc3Npb24gZnJvbSBvdGhlciBhbmltYWxzLCB3aGljaCBpcyBhIGx1c3Qgb2Yg" +
      "dGhlIG1pbmQsIHRoYXQgYnkgYSBwZXJzZXZlcmFuY2Ugb2YgZGVsaWdodCBpbiB0aGUgY29udGlu" +
      "dWVkIGFuZCBpbmRlZmF0aWdhYmxlIGdlbmVyYXRpb24gb2Yga25vd2xlZGdlLCBleGNlZWRzIHRo" +
      "ZSBzaG9ydCB2ZWhlbWVuY2Ugb2YgYW55IGNhcm5hbCBwbGVhc3VyZS4="
    )
  }
}
