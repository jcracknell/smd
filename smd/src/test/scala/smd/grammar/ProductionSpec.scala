package smd
package grammar

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FunSpec
import smd.parsing.ParsingResult

trait ProductionSpec extends FunSpec with ShouldMatchers {
  import scala.language.reflectiveCalls

  def subject: Parser[_]

  def shouldParse(input: String) = new {
    def as[A](expected: A) = new {
      private var result: ParsingResult[A] = _

      it(s"should parse ${input.literalEncode} as $expected") {
        result = subject.asInstanceOf[Parser[A]].parse(input)

        if(result.failed) fail(s"Parsing failed.")

        if(result.length != input.length) fail(s"Production did not consume entire input.")

        result.product should be (expected)
      }
    }
  }

  implicit class TestableParser[+A](parser: Parser[A]) {
    def shouldParse(input: String): Unit = ???
    def shouldNotParse(input: String): Unit = ???
  }
}
