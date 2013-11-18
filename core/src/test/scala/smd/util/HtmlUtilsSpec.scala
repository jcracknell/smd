package smd
package util

import org.scalatest.{Matchers, FunSpec}

class HtmlUtilsSpec extends FunSpec with Matchers {
  describe("encodeUriComponent") {
    List(
      "A"      -> "A",
      " "      -> "%20",
      "a b"    -> "a%20b",
      "À"      -> "%C3%80",
      "\u30a2" -> "%E3%82%A2" // KATAKANA LETTER A
    ) foreach { case (input, expected) =>
      it(s"should encode ${input.literalEncode} as ${expected.literalEncode}") {
        HtmlUtils.encodeUriComponent(input) should be (expected)
      }
    }
  }
}
