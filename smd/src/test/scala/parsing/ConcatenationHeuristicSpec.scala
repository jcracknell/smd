package smd
package parsing

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FunSpec

class ConcatenationHeuristicSpec extends FunSpec with ShouldMatchers with Parsers {
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
      SequenceParser3[Option[String], String, String](
        SequenceParser(
          LiteralParser("a"),
          LiteralParser("b"),
          LiteralParser("c")))
    )
  }
  it("SequenceParser3[A, B, C] ~ Parser[D] => SequenceParser") {
    val a = SequenceParser3[String, String, String](
              SequenceParser(
                LiteralParser("a"),
                LiteralParser("b"),
                LiteralParser("c")))

    val b = LiteralParser("d")

    (a ~ b) should be (
      SequenceParser(
        LiteralParser("a"),
        LiteralParser("b"),
        LiteralParser("c"),
        LiteralParser("d"))
    )
  }
  it("SequenceParser ~ Parser[_] -> SequenceParser") {
    val a = SequenceParser(LiteralParser("a"))
    val b = LiteralParser("b")

    (a ~ b) should be (
      SequenceParser(
        LiteralParser("a"),
        LiteralParser("b"))
    )
  }
  it("LiteralParser ~ LiteralParser => LiteralParser") {
    (LiteralParser("a") ~ LiteralParser("b")) should be (LiteralParser("ab"))
  }
}
