package smd
package util

import scala.util.Try
import scala.annotation.tailrec

trait NumeralSystem {
  /** The minimum value representable in this [[smd.util.NumeralSystem]]. */
  def minValue: Int
  /** The maximum value representable in this [[smd.util.NumeralSystem]]. */
  def maxValue: Int
  def encode(i: Int): Option[String]
  def decode(s: CharSequence, start: Int, end: Int): Option[Int]
  def decode(s: CharSequence): Option[Int] = decode(s, 0, s.length)
}

object NumeralSystem {
  object Arabic extends Arabic
  object Alpha extends Alpha
  object Roman extends Roman

  /** [[smd.util.NumeralSystem]] implementation for lowercase alpha numerals. */
  trait Alpha extends NumeralSystem {
    val minValue: Int = 1
    val maxValue: Int = Int.MaxValue

    def encode(i: Int): Option[String] = {
      def encodeImpl(i: Int): List[Char] =
        if(i < 0) Nil else (97 + i % 26).toChar :: encodeImpl(i / 26 - 1)

      Option.when(minValue <= i && i <= maxValue) { encodeImpl(i - 1).reverse.mkString }
    }

    def decode(s: CharSequence, start: Int, end: Int): Option[Int] = {
      require(start <= end, "start must be greater than end")

      def decodeImpl(acc: Int, pos: Int): Option[Int] =
        if(pos == end) Some(acc)
        else {
          val c = s.charAt(pos).toLower
          Option.when('a' <= c && c <= 'z')(c - 96) flatMap { d => decodeImpl(acc * 26 + d, pos + 1) }
        }

      decodeImpl(0, start)
    }
  }

  /** [[smd.util.NumeralSystem]] implementation for arabic numerals. */
  trait Arabic extends NumeralSystem {
    val minValue = Int.MinValue
    val maxValue = Int.MaxValue
    def encode(i: Int): Option[String] = Some(i.toString)
    def decode(s: CharSequence, start: Int, end: Int): Option[Int] =
      try { Some(java.lang.Integer.parseInt(s.toString.substring(start, end))) } catch { case _: Throwable => None }
  }

  /** [[smd.util.NumeralSystem]] implementation for lowercase roman numerals. */
  trait Roman extends NumeralSystem {
    val minValue: Int = 1
    val maxValue: Int = 3999
    def encode(i: Int): Option[String] = ???
    def decode(s: CharSequence, start: Int, end: Int): Option[Int] = ???
  }
}
