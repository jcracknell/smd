package smd
package parsing

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FunSpec

class SequencingHeuristicSpec extends FunSpec with ShouldMatchers with Parsers {
  it("Parser[A] ~ Parser[B] => SequenceParser2[A, B]") {
    val a = OptionalParser(LiteralParser("a"))
    val b = LiteralParser("b")
    val c = a ~ b
    c.toString

    (a ~ b) should be (
      SequenceParser2(OptionalParser(LiteralParser("a")), LiteralParser("b"))
    )
  }
  it("SequenceParser2[A, B] ~ Parser[C] => SequenceParser3[A, B, C]") {
    val a = SequenceParser2( LiteralParser("a"), LiteralParser("b"))
    val b = LiteralParser("c")

    (a ~ b) should be (
      SequenceParser3(LiteralParser("a"), LiteralParser("b"), LiteralParser("c"))
    )
  }
  it("SequenceParser3[A, B, C] ~ Parser[D] => SequenceParser4[A, B, C, D]") {
    val a = SequenceParser3(LiteralParser("a"), LiteralParser("b"), LiteralParser("c"))
    val b = LiteralParser("d")

    (a ~ b) should be (
      SequenceParser4(LiteralParser("a"), LiteralParser("b"), LiteralParser("c"), LiteralParser("d"))
    )
  }
  it("SequenceParser ~ Parser[_] -> SequenceParser") {
    val a = SequenceParser(LiteralParser("a"), LiteralParser("b"))
    val b = LiteralParser("c")

    (a ~ b) should be (
      SequenceParser(
        LiteralParser("a"),
        LiteralParser("b"),
        LiteralParser("c"))
    )
  }
  it("OptionalParser[String] ~ \"b\" => SequenceParser2[Option[String], String]") {
    (OptionalParser(LiteralParser("a")) ~ "b") should be (
      SequenceParser2(OptionalParser(LiteralParser("a")), LiteralParser("b"))
    )
  }
}
