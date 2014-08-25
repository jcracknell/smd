package smd
package util

import org.scalatest.{FunSpec, Matchers}

class LazySpec extends FunSpec with Matchers {
  it("should actually be lazy") {
    val lzy = Lazy { throw new Exception } 
    an [Exception] should be thrownBy (lzy.get)
  }
  it("should be implicitly creatable") {
    val lzy: Lazy[Int] = 42
    lzy.get should be (42)
  }

  describe("mapping") {
    it("should work as expected") {
      Seq(1, 2).map(Lazy.mapping(i => 2*i)).map(_.get) should matchPattern { case Seq(2, 4) => }
    }
    it("should actually be lazy") {
      val lzy = Seq(1, 2).map(Lazy.mapping(i => throw new Exception))
      an [Exception] should be thrownBy (lzy.head.get)
    }
  }
}
