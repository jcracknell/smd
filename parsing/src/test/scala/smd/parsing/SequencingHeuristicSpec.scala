package smd
package parsing

import org.scalatest.{Matchers, FunSpec}
import smd.parsing

class SequencingHeuristicSpec extends FunSpec with Matchers with Parsers {
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
  it("SequenceParser[A, B] ~ SequenceParser[C, D] -> SequenceParser[A, B, C, D]") {
    val a = SequenceParser2(LiteralParser("a"), LiteralParser("b"))
    val b = SequenceParser2(LiteralParser("c"), LiteralParser("d"))

    (a ~ b) should be (
      SequenceParser4(
        LiteralParser("a"),
        LiteralParser("b"),
        LiteralParser("c"),
        LiteralParser("d")
      )
    )
  }
  it("SequenceParser ~ Parser -> SequenceParser") {
    (SequenceParser(LiteralParser("a"), LiteralParser("b")) ~ LiteralParser("c")) should be (
      SequenceParser(LiteralParser("a"), LiteralParser("b"), LiteralParser("c"))
    )
  }
  it("Parser ~ SequenceParser -> SequenceParser") {
    (LiteralParser("a") ~ SequenceParser(LiteralParser("b"), LiteralParser("c"))) should be (
      SequenceParser(LiteralParser("a"), LiteralParser("b"), LiteralParser("c"))
    )
  }
  it("SequenceParser ~ SequenceParser -> SequenceParser") {
    val a = SequenceParser(LiteralParser("a"), LiteralParser("b"))
    val b = SequenceParser(LiteralParser("c"), LiteralParser("d"))
    (a ~ b) should be (SequenceParser(LiteralParser("a"), LiteralParser("b"), LiteralParser("c"), LiteralParser("d")))
  }
  it("SequenceParser ~ SequenceParser2[A, B] -> SequenceParser") {
    val a = SequenceParser(LiteralParser("a"), LiteralParser("b"))
    val b = SequenceParser2(LiteralParser("c"), LiteralParser("d"))
    (a ~ b) should be (SequenceParser(LiteralParser("a"), LiteralParser("b"), LiteralParser("c"), LiteralParser("d")))
  }
  it("SequenceParser2[A, B] ~ SequenceParser -> SequenceParser") {
    val a = SequenceParser2(LiteralParser("a"), LiteralParser("b"))
    val b = SequenceParser(LiteralParser("c"), LiteralParser("d"))
    (a ~ b) should be (SequenceParser(LiteralParser("a"), LiteralParser("b"), LiteralParser("c"), LiteralParser("d")))
  }
}
