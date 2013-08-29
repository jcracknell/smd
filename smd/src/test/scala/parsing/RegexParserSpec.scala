package smd
package parsing

import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers
import smd.unicode.GraphemeExemplars

class RegexParserSpec extends FunSpec with ShouldMatchers with GraphemeExemplars {
  it("should match") {
    (RegexParser("[a-z]+".r).parse(new ParsingContext("abc123")) match {
      case Success(_, 0, 3) => true
      case _ => false
    }) should be (true)
  }
  it("should match at the end of the input") {
    (RegexParser("[a-z]+".r).parse(new ParsingContext("123abc", 3)) match {
      case Success(_, 3, 3) => true
      case _ => false
    }) should be (true)
  }
  it("should only match a prefix") {
    (RegexParser("[a-z]+").parse(new ParsingContext("1a"))) shouldEqual (Failure)
  }
  it("should not match a partial grapheme") {
    (RegexParser("[a-z]+".r).parse(new ParsingContext(s"a${g.combining_acute_accent}"))) shouldEqual (Failure)
  }
}
