package smd
package grammar

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FunSpec
import smd.parsing.ParsingResult

trait ProductionSpec extends FunSpec with ShouldMatchers {
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
}
