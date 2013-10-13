package smd
package grammar

import org.scalatest.FunSpec

trait ProductionSpec extends FunSpec {
  // Nifty aliases
  type Parser[+A]        = smd.parsing.Parser[A]
  type ParsingResult[+A] = smd.parsing.ParsingResult[A]
  val Accepted           = smd.parsing.Accepted
  val Rejected           = smd.parsing.Rejected

  //region ParsingScenario

  def parsing(input: String): ParsingScenarioInput = ParsingScenarioInput(input)

  case class ParsingScenarioInput(input: String) {
    def as[A](parser: Parser[A]): ParsingScenario[A] = ParsingScenario(input, parser)
  }

  case class ParsingScenario[+A](input: String, parser: Parser[A]) {
    def should(expectation: ParsingExpectation[A]): Unit = {
      it(s"parsing ${input.literalEncode} should ${expectation.summary}") {
        expectation.assert(this)
      }
    }
  }

  //endregion

  //region ParsingExpectation

  def produce[A](expectedProduct: A): ParsingExpectation[A] =
    new ParsingExpectation.Product(expectedProduct)

  def produceMatching[A](productMatch: PartialFunction[A, Unit]): ParsingExpectation[A] =
    new ParsingExpectation.ProductMatch(productMatch)

  val reject: ParsingExpectation[Any] = ParsingExpectation.Rejection

  trait ParsingExpectation[-A] {
    def summary: String
    def assert(scenario: ParsingScenario[A]): Unit
  }

  object ParsingExpectation {
    class Product[-A](expectedProduct: A) extends ParsingExpectation[A] {
      def summary: String = s"produce $expectedProduct"
      def assert(scenario: ParsingScenario[A]): Unit =
        scenario.parser.parse(scenario.input) match {
          case Accepted.Producing(actualProduct) =>
            if(expectedProduct != actualProduct)
              fail(s"expected: $expectedProduct\nactual: $actualProduct")
          case _ =>
            fail("input was rejected")
        }
    }

    class ProductMatch[-A](productExpectation: PartialFunction[A, Unit]) extends ParsingExpectation[A] {
      def summary: String = "accept with product match"

      def assert(scenario: ParsingScenario[A]): Unit =
        scenario.parser.parse(scenario.input) match {
          case Rejected => fail(s"input was rejected")
          case Accepted.Producing(product) =>
            if(!productExpectation.isDefinedAt(product))
              fail(s"no match: $product")
            else
              productExpectation(product)
        }
    }

    object Rejection extends ParsingExpectation[Any] {
      def summary: String = "reject the input."
      def assert(scenario: ParsingScenario[Any]): Unit =
        scenario.parser.parse(scenario.input) match {
          case Accepted.Producing(product) =>
            fail(s"accepted producing $product")
          case _ =>
        }
    }
  }

  //endregion
}
