package smd
package parsing

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FunSpec

class SequencingHeuristicSpec extends FunSpec with ShouldMatchers with Parsers {
  it("Parser[A] ~ Parser[B] => SequenceParser2[A, B]") {
    val a = OptionalParser(LiteralParser("a"))
    val b = LiteralParser("b")

    (a ~ b) should be (
      SequenceParser2[Option[String], String](
        SequenceParser(
          OptionalParser(LiteralParser("a")),
          LiteralParser("b")))
    )
  }
  it("SequenceParser2[A, B] ~ Parser[C] => SequenceParser3[A, B, C]") {
    val a = SequenceParser2[String, String](
              SequenceParser(
                LiteralParser("a"),
                LiteralParser("b")))

    val b = LiteralParser("c")

    (a ~ b) should be (
      SequenceParser3[String, String, String](
        SequenceParser(
          LiteralParser("a"),
          LiteralParser("b"),
          LiteralParser("c")))
    )
  }
  it("SequenceParser3[A, B, C] ~ Parser[D] => SequenceParser4[A, B, C, D]") {
    val a = SequenceParser3[String, String, String](
              SequenceParser(
                LiteralParser("a"),
                LiteralParser("b"),
                LiteralParser("c")))

    val b = LiteralParser("d")

    (a ~ b) should be (
      SequenceParser4[String, String, String, String](
        SequenceParser(
          LiteralParser("a"),
          LiteralParser("b"),
          LiteralParser("c"),
          LiteralParser("d")))
    )
  }
  it("SequenceParser4[A, B, C, D] ~ Parser[E] => SequenceParser") {
    val a = SequenceParser4[String, String, String, String](
      SequenceParser(
        LiteralParser("a"),
        LiteralParser("b"),
        LiteralParser("c"),
        LiteralParser("d")))

    val b = LiteralParser("e")

    (a ~ b) should be (
      SequenceParser(
        LiteralParser("a"),
        LiteralParser("b"),
        LiteralParser("c"),
        LiteralParser("d"),
        LiteralParser("e"))
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
  it("LiteralParser ~ LiteralParser => LiteralParser") {
    (LiteralParser("a") ~ LiteralParser("b")) should be (LiteralParser("ab"))
  }
  it("\"a\" ~ \"b\" => LiteralParser") {
    ("a" ~ "b") should be (LiteralParser("ab"))
  }
  it("OptionalParser[String] ~ \"b\" => SequenceParser2[Option[String], String]") {
    (OptionalParser(LiteralParser("a")) ~ "b") should be (
      SequenceParser2[Option[String], String](
        SequenceParser(
          OptionalParser(LiteralParser("a")),
          LiteralParser("b")))
    )
  }
}
