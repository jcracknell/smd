package smd
package util

import org.scalatest.{FunSpec, Matchers}

class LazySpec extends FunSpec with Matchers {
  it("should be implicitly creatable") {
    val lzy: Lazy[Int] = 42
    lzy.get should be (42)
  }

  describe("get") {
    it("should work as expected") {
      Lazy(42).get should be (42)
    }
    it("should be lazy") {
      val lzy = Lazy { throw new Exception } 
      an [Exception] should be thrownBy (lzy.get)
    }
    it("should only evaluate once") {
      var evaluationCount = 0
      val lzy = Lazy { evaluationCount += 1; 42 }
      evaluationCount should be (0)
      lzy.get should be (42)
      evaluationCount should be (1)
      lzy.get should be (42)
      evaluationCount should be (1)
    }
  }

  describe("map") {
    it("should work as expected") {
      Lazy(12).map(_ * 2).get should be (24)
    }
    it("should be lazy") {
      val a = Lazy[Int] { throw new Exception }
      val b = a.map(_ * 2)
      an [Exception] should be thrownBy (b.get)
    }
  }

  describe("toString") {
    it("should return a generic value") {
      Lazy("foo").toString should be ("Lazy(?)")
    }
    it("should not force evaluation") {
      var evaluated = false
      (Lazy { evaluated = true; "foo" }).toString
      evaluated should be (false)
    }
  }

  describe("mapping") {
    it("should work as expected") {
      Seq(1, 2).map(Lazy.mapping(_ * 2)).map(_.get) should matchPattern { case Seq(2, 4) => }
    }
    it("should be lazy") {
      val seq = Seq(1, 2).map(Lazy.mapping(i => throw new Exception))
      an [Exception] should be thrownBy (seq.head.get)
    }
  }
}
