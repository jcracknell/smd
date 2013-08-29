package smd
package unicode

import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers

class GraphemeInfoSpec extends FunSpec with ShouldMatchers with GraphemeExemplars {
  describe("at method") {
    def resultFor(str: String, index: Int = 0)(assert: GraphemeInfo => Unit): Unit = {
      it(s"should produce the expected result for ${str.literalEncode} at index $index") {
        assert(GraphemeInfo.at(str, index))
      }
    }

    resultFor("a") { _ shouldEqual GraphemeInfo(0, 1, Character.LOWERCASE_LETTER) }
    resultFor("ab") { _ shouldEqual GraphemeInfo(0, 1, Character.LOWERCASE_LETTER) }
    resultFor(s"a${g.combining_ring}") { _ shouldEqual GraphemeInfo(0, 2, Character.LOWERCASE_LETTER) }
    resultFor(s"a${g.combining_ring}${g.combining_caron}") { _ shouldEqual GraphemeInfo(0, 3, Character.LOWERCASE_LETTER) }
    resultFor(s"${g.combining_ring}a${g.combining_ring}") { _ shouldEqual GraphemeInfo(0, 1, Character.NON_SPACING_MARK) }
  }
}
