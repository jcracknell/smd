package smd
package util

import org.scalatest.{Inside, FunSpec, Matchers}

class CompositeCharSequenceSpec extends FunSpec with Matchers with PatternMatchers {
  /** Produce a string representation of the provided [[java.lang.CharSequence]], possibly a
    * [[smd.util.CompositeCharSequence]], as `toString` an a [[java.lang.CharSequence]] must return
    * the equivalent string representation. */
  def treeString(cs: CharSequence): String = cs match {
    case ccs: CompositeCharSequence =>
      s"${ccs.getClass.getSimpleName}(${treeString(ccs.left)},${treeString(ccs.right)})"
    case _ =>
      cs.toString.literalEncode
  }

  describe("balanced construction") {
    it("should work for 2 parts") {
      val cs = CompositeCharSequence.balanced(Seq("one", "two"))
      cs.toString should be ("onetwo")

      cs should matchPattern {
        case CompositeCharSequence("one", "two") => matched
      }
    }
    it("should work for 3 parts") {
      val cs = CompositeCharSequence.balanced(Seq("one", "two", "three"))
      cs.toString should be ("onetwothree")

      cs should matchPattern {
        case CompositeCharSequence(
          CompositeCharSequence("one", "two"),
          "three"
        ) => matched
      }
    }
    it("should work for 4 parts") {
      val cs = CompositeCharSequence.balanced(Seq("one", "two", "three", "four"))
      cs.toString should be ("onetwothreefour")

      cs should matchPattern {
        case CompositeCharSequence(
          CompositeCharSequence("one", "two"),
          CompositeCharSequence("three", "four")
        ) => matched
      }
    }
    it("should work for 5 parts") {
      val cs = CompositeCharSequence.balanced(Seq("one","two","three","four","five"))
      cs.toString should be ("onetwothreefourfive")
    }
    it("should work for 6 parts") {
      val cs = CompositeCharSequence.balanced(Seq("one", "two", "three", "four", "five", "six"))
      cs.toString should be ("onetwothreefourfivesix")

      cs should matchPattern {
        case CompositeCharSequence(
          CompositeCharSequence(
            CompositeCharSequence("one", "two"),
            "three"
          ),
          CompositeCharSequence(
            CompositeCharSequence("four", "five"),
            "six"
          )
        ) => matched
      }
    }
  }
  describe("weighted construction") {
    it("should work for two parts")  {
      val cs = CompositeCharSequence.weighted(Seq("one", "two"), balancedBelow = 0)
      cs.toString should be ("onetwo")
    }
    it("should work for 3 parts") {
      val cs = CompositeCharSequence.weighted(Seq("one","two","three"), balancedBelow = 0)
      cs.toString should be ("onetwothree")
    }
    it("should work for 4 parts")  {
      val cs = CompositeCharSequence.weighted(Seq("one","two","three","four"), balancedBelow = 0)
      cs.toString should be ("onetwothreefour")
    }
    it("should work for 5 parts") {
      val cs = CompositeCharSequence.weighted(Seq("one","two","three","four","five"), balancedBelow = 0)
      cs.toString should be ("onetwothreefourfive")
    }
    it("should work for 6 parts") {
      val cs = CompositeCharSequence.weighted(Seq("one","two","three","four","five","six"), balancedBelow = 0)
      cs.toString should be ("onetwothreefourfivesix")
    }
    it("should construct the expected tree 1") {
      CompositeCharSequence.weighted(Seq("abcd", "a", "b", "c"), balancedBelow = 0) should matchPattern {
        case CompositeCharSequence(
          "abcd",
          CompositeCharSequence(
            "a",
            CompositeCharSequence("b", "c")
          )
        ) => matched
      }
      CompositeCharSequence.weighted(Seq("a","b", "c","abcd"), balancedBelow = 0) should matchPattern {
        case CompositeCharSequence(
          CompositeCharSequence(
            "a",
            CompositeCharSequence("b", "c")
          ),
          "abcd"
        ) => matched
      }
    }
    it("should construct the expected tree 2") {
      val left = CompositeCharSequence.weighted(Seq("abcdefgh", "a", "b", "c", "d", "e", "f", "g", "h"), balancedBelow = 0)
      left.toString should be ("abcdefghabcdefgh")
      left should matchPattern { case CompositeCharSequence("abcdefgh", _) => matched }

      val right = CompositeCharSequence.weighted(Seq("a", "b", "c", "d", "e", "f", "g", "h", "abcdefgh"), balancedBelow = 0)
      right.toString should be ("abcdefghabcdefgh")
      right should matchPattern { case CompositeCharSequence(_, "abcdefgh") => matched }
    }
  }
}
