package smd
package parsing

import org.scalatest.{FunSpec, Matchers}

class LongestChoiceParserSpec extends FunSpec with Matchers {
  it("should accept the longest match") {
    (LongestChoiceParser(
      RegexParser("a+")      ^^^ 1,
      RegexParser("[a-z]+0") ^^^ 2,
      RegexParser("[a-z]+")  ^^^ 3
    ).parse("aaab0")) should matchPattern { case Accepted.Producing(2) => }
  }
  it("should produce the first of several equal-length matches") {
    (LongestChoiceParser(
      RegexParser("[a-z]+") ^^^ 1,
      RegexParser("a+")     ^^^ 2
    ).parse("aaa0")) should matchPattern { case Accepted.Producing(1) => }
  }
}
