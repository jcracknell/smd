package smd
package parsing

import org.scalatest.{Matchers, FunSpec}
import smd.unicode.GraphemeExemplars

class RegexParserSpec extends FunSpec with Matchers with GraphemeExemplars {
  object SuccessAt {
    def unapply[A](pr: ParsingResult[A]): Option[(A, Int, Int)] =
      if(pr.rejected) None else Some((pr.product, pr.index, pr.length))
  }

  it("should match") {
    (RegexParser("[a-z]+".r).parse(ParsingContext("abc123")) match {
      case SuccessAt(_, 0, 3) => true
      case _ => false
    }) should be (true)
  }
  it("should match at the end of the input") {
    (RegexParser("[a-z]+".r).parse(ParsingContext("123abc", 3)) match {
      case SuccessAt(_, 3, 3) => true
      case _ => false
    }) should be (true)
  }
  it("should only match a prefix") {
    (RegexParser("[a-z]+").parse(ParsingContext("1a")).accepted) should be (false)
  }
  it("should not match a partial grapheme") {
    (RegexParser("[a-z]+".r).parse(ParsingContext(s"a${g.combining_acute_accent}")).accepted) should be (false)
  }
}
