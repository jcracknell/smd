package smd
package parsing

import org.scalatest.{Matchers, FunSpec}

class InputExtentSpec extends FunSpec with Matchers {
  import scala.language.implicitConversions
  implicit def charSequenceToSourceExtent(cs: CharSequence): InputExtent = InputExtent(cs)

  val abcdabcd = InputExtent.Root("abcdabcd")

  describe("Root") {
    describe("equality") {
      it("should implement CharSequence equality") {
        (InputExtent.Root("abcd") == "abcd") should be (true)
      }
    }
  }

  describe("SubSequence") {
    describe("subSequence") {
      it("should not create additional levels") {
        InputExtent.SubSequence(abcdabcd, 2, 6).subSequence(1, 2) should matchPattern {
          case InputExtent.SubSequence(InputExtent("abcdabcd"), 3, 4) =>
        }
      }
    }
    describe("equals") {
      it("should implement CharSequence equality") {
        (InputExtent.SubSequence(abcdabcd, 0, 4) == InputExtent.SubSequence(abcdabcd, 4, 8)) should be (true)
      }
    }
  }

  describe("Composite") {
    val source = InputExtent.Root("012345678")
    describe("subSequence") {
      val subject = InputExtent.Composite(InputExtent.SubSequence(abcdabcd, 0, 2), InputExtent.SubSequence(abcdabcd, 6, 8))
      it("should work for the general case") {
        subject.subSequence(1, 3) should matchPattern {
          case InputExtent.SubSequence(
            InputExtent.Composite(
              InputExtent.SubSequence(InputExtent("abcdabcd"), 0, 2),
              InputExtent.SubSequence(InputExtent("abcdabcd"), 6, 8)
            ),
            1, 3
          ) =>
        }
      }
      it("should optimize a sub-sequence contained entirely by the left side") {
        subject.subSequence(0, 1) should matchPattern { case InputExtent.SubSequence(InputExtent("abcdabcd"), 0, 1) => }
      }
      it("should optimize a sub-sequence contained entirely by the right side") {
        subject.subSequence(3, 4) should matchPattern { case InputExtent.SubSequence(InputExtent("abcdabcd"), 7, 8) => }
      }
    }
    describe("equals") {
      it("should implement CharSequence equality") {
        val subject = InputExtent.Composite(InputExtent.SubSequence(abcdabcd, 0, 2), InputExtent.SubSequence(abcdabcd, 6, 8))
        (subject == "abcd") should be (true)
      }
    }
    describe("balanced construction") {
      it("should work for 0 parts") {
        InputExtent.Composite.balanced(Seq()) should matchPattern { case InputExtent("") => }
      }
      it("should work for 1 part") {
        InputExtent.Composite.balanced(IndexedSeq(
          source.subSequence(0, 1)
        )) should matchPattern { case
          InputExtent("0") =>
        }
      }
      it("should work for 2 parts") {
        InputExtent.Composite.balanced(IndexedSeq(
          source.subSequence(0, 1),
          source.subSequence(1, 2)
        )) should matchPattern { case
          InputExtent.Composite(
            InputExtent("0"),
            InputExtent("1")
          ) =>
        }
      }
      it("should work for 3 parts") {
        InputExtent.Composite.balanced(IndexedSeq(
          source.subSequence(0, 1),
          source.subSequence(1, 2),
          source.subSequence(2, 3)
        )) should matchPattern { case
          InputExtent.Composite(
            InputExtent.Composite(
              InputExtent("0"),
              InputExtent("1")
            ),
            InputExtent("2")
          ) => 
        }
      }
      it("should work for 4 parts") {
        InputExtent.Composite.balanced(IndexedSeq(
          source.subSequence(0, 1),
          source.subSequence(1, 2),
          source.subSequence(2, 3),
          source.subSequence(3, 4)
        )) should matchPattern { case
          InputExtent.Composite(
            InputExtent.Composite(
              InputExtent("0"),
              InputExtent("1")
            ),
            InputExtent.Composite(
              InputExtent("2"),
              InputExtent("3")
            )
          ) => 
        }
      }
    }
  }
}
