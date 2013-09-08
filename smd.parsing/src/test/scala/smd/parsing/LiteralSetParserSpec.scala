package smd
package parsing

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FunSpec

class LiteralSetParserSpec extends FunSpec with ShouldMatchers {
  it("should match the longest literal") {
    LiteralSetParser("foo", "foobar").parse("foobared").product should be ("foobar")
  }
  it("should match a literal at the end of the input") {
    LiteralSetParser("foo", "bar", "baz").parse("foo").product should be("foo")
  }
  it("should match a shorter literal when the input partially matches a longer one") {
    LiteralSetParser("foo", "foobar").parse("foobore").product should be ("foo")
  }
  it("should yield the correct product") {
    val parser = LiteralSetParser("foo" -> 1, "bar" -> 2)
    parser.parse("foo").product should be (1)
    parser.parse("bar").product should be (2)
  }
}
