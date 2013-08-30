package smd
package parsing

import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers
import smd.unicode.GraphemeExemplars

class RegexParserSpec extends FunSpec with ShouldMatchers with GraphemeExemplars {
  object SuccessAt {
    def unapply[A](pr: ParsingResult[A]): Option[(A, Int, Int)] =
      if(pr.failed) None else Some((pr.product, pr.index, pr.length))
  }

  it("should match") {
    (RegexParser("[a-z]+".r).parse(new ParsingContext("abc123")) match {
      case SuccessAt(_, 0, 3) => true
      case _ => false
    }) should be (true)
  }
  it("should match at the end of the input") {
    (RegexParser("[a-z]+".r).parse(new ParsingContext("123abc", 3)) match {
      case SuccessAt(_, 3, 3) => true
      case _ => false
    }) should be (true)
  }
  it("should only match a prefix") {
    (RegexParser("[a-z]+").parse(new ParsingContext("1a")).succeeded) should be (false)
  }
  it("should not match a partial grapheme") {
    (RegexParser("[a-z]+".r).parse(new ParsingContext(s"a${g.combining_acute_accent}")).succeeded) should be (false)
  }
}
