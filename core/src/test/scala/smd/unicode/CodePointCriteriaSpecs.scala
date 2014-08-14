package smd
package unicode

import org.scalatest.{Matchers, FunSpec}

class CodePointCriteriaSpecs extends FunSpec with Matchers {
  import CodePointCriteria._

  def satisfyingCodePoints(criterion: CodePointCriterion): Set[CodePointInfo] =
    (Character.MIN_CODE_POINT to Character.MAX_CODE_POINT)
    .map(cp => CodePointInfo.of(cp))
    .filter(cpi => criterion.isSatisfiedBy(cpi))
    .toSet

  describe("Any") {
    it("should be satisfied by every code point") {
      val numCodePoints = Character.MAX_CODE_POINT - Character.MIN_CODE_POINT + 1
      satisfyingCodePoints(Any).size should be (numCodePoints)
    }
  }
  describe("None") {
    it("should not be satisfied by any valid code point") {
      satisfyingCodePoints(None) should be (Set())
    }
  }
  describe("Values") {
    it("should only be satisfied by listed code points") {
      satisfyingCodePoints(Values('a', 'b')).map(_.value) should be (Set[Int]('a', 'b'))
    }
  }
}
