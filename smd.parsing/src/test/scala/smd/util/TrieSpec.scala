package smd
package util

import org.scalatest.{Matchers, FunSpec}

class TrieSpec extends FunSpec with Matchers {
  describe("construction") {
    it("should yield the expected structure") {
      Trie("" -> 1, "at" -> 2, "bat" -> 3, "a" -> 4) should be(
        Trie(Some(1), Map(
          'a' -> Trie(Some(4), Map(
            't' -> Trie(Some(2))
          )),
          'b' -> Trie(None, Map(
            'a' -> Trie(None, Map(
              't' -> Trie(Some(3))
            ))
          ))
        ))
      )
    }
  }
  describe("values method") {
    it("should retrieve the values on the specified path in the correct order") {
      Trie("" -> 1, "at" -> 2, "bat" -> 3, "a" -> 4).values("at") should be (Seq(("", 1), ("a", 4), ("at", 2)))
    }
    it("should work for an empty trie") {
      Trie().values("foobar") should be (Seq())
    }
  }
  describe("Iterable implementation") {
    it("should retrieve all values") {
      (Trie("" -> 1, "at" -> 2, "bat" -> 3, "a" -> 4).toSet) should be (Set(
        "" -> 1, "at" -> 2, "bat" -> 3, "a" -> 4
      ))
    }
  }
}
