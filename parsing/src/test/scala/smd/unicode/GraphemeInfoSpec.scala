package smd
package unicode

import org.scalatest.{Matchers, FunSpec}

class GraphemeInfoSpec extends FunSpec with Matchers with GraphemeExemplars {
  describe("at method") {
    def test[A](str: String, index: Int = 0)(assert: PartialFunction[GraphemeInfo, A]): Unit = {
      it(s"should produce the expected result for ${str.literalEncode} at index $index") {
        val result = GraphemeInfo.at(str, index)
        assert.isDefinedAt(result) should be (true)
        assert.apply(result)
      }
    }

    test("a") {
      case GraphemeInfo(0, 1, Character.LOWERCASE_LETTER, Seq(_)) => true
    }
    test("ab") {
      case GraphemeInfo(0, 1, Character.LOWERCASE_LETTER, Seq(_)) => true
    }
    test(s"a${g.combining_ring}") {
      case GraphemeInfo(0, 2, Character.LOWERCASE_LETTER, Seq(CodePointInfo('a'), _)) =>
    }
    test(s"a${g.combining_ring}${g.combining_caron}") {
      case GraphemeInfo(0, 3, Character.LOWERCASE_LETTER, Seq(CodePointInfo('a'), _, _)) =>
    }
    test(s"${g.combining_ring}a${g.combining_ring}") {
      case GraphemeInfo(0, 1, Character.NON_SPACING_MARK, Seq(_)) => true
    }
  }
}
