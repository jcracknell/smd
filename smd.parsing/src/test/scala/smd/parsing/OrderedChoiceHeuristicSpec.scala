package smd.parsing

import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers

class OrderedChoiceHeuristicSpec extends FunSpec with ShouldMatchers {
  it("Parser | Parser") {
    val a = LiteralParser("a")
    val b = LiteralParser("b")

    (a | b) should be (
      OrderedChoiceParser[String](
        LiteralParser("a"),
        LiteralParser("b")
      )
    )
  }
  it("OrderedChoiceParser | Parser") {
    val a = OrderedChoiceParser(LiteralParser("a"), LiteralParser("b"))
    val b = LiteralParser("c")

    (a | b) should be (
      OrderedChoiceParser[String](
        LiteralParser("a"),
        LiteralParser("b"),
        LiteralParser("c")
      )
    )
  }
  it("Parser | OrderedChoiceParser") {
    val a = LiteralParser("a")
    val b = OrderedChoiceParser(LiteralParser("b"), LiteralParser("c"))

    (a | b) should be (
      OrderedChoiceParser[String](
        LiteralParser("a"),
        LiteralParser("b"),
        LiteralParser("c")
      )
    )
  }
  it("OrderedChoiceParser | OrderedChoiceParser") {
    val a = OrderedChoiceParser(LiteralParser("a"), LiteralParser("b"))
    val b = OrderedChoiceParser(LiteralParser("c"), LiteralParser("d"))

    (a | b) should be (
      OrderedChoiceParser[String](
        LiteralParser("a"),
        LiteralParser("b"),
        LiteralParser("c"),
        LiteralParser("d")
      )
    )
  }
}
