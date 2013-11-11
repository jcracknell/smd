package smd
package util

import scala.util.Try
import scala.annotation.{switch, tailrec}
import smd.parsing.{Parser, Parsers}

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
  /** [[smd.util.NumeralSystem]] implementation for lowercase alpha numerals. */
  object Alpha extends NumeralSystem {
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
  object Arabic extends NumeralSystem {
    val minValue = Int.MinValue
    val maxValue = Int.MaxValue
    def encode(i: Int): Option[String] = Some(i.toString)
    def decode(s: CharSequence, start: Int, end: Int): Option[Int] =
      try { Some(java.lang.Integer.parseInt(s.toString.substring(start, end))) } catch { case _: Throwable => None }
  }

  /** [[smd.util.NumeralSystem]] implementation for lowercase roman numerals. */
  object Roman extends NumeralSystem with Parsers {
    val minValue: Int = 1
    val maxValue: Int = 3999

    def encode(i: Int): Option[String] = Option.when(minValue <= i && i <= maxValue) {
      val sb = new StringBuilder(15)

      @inline def encodeDecade(v: Int, d: Char, q: Char, u: Char): Unit =
        (v: @switch @unchecked) match {
          case 0 => sb
          case 1 => sb + u
          case 2 => sb + u + u
          case 3 => sb + u + u + u
          case 4 => sb + u + q
          case 5 => sb + q
          case 6 => sb + q + u
          case 7 => sb + q + u + u
          case 8 => sb + q + u + u + u
          case 9 => sb + u + d
        }

      encodeDecade(i / 1000,      '?', '?', 'm')
      encodeDecade(i /  100 % 10, 'm', 'd', 'c')
      encodeDecade(i /   10 % 10, 'c', 'l', 'x')
      encodeDecade(i        % 10, 'x', 'v', 'i')
      sb.toString
    }

    def decode(s: CharSequence, start: Int, end: Int): Option[Int] =
      grammar.parse(s.subSequenceProxy(start, end)).productOption

    protected val grammar = {
      val m = ("m" | "M") ^^^ 1000
      val d = ("d" | "D") ^^^  500
      val c = ("c" | "C") ^^^  100
      val l = ("l" | "L") ^^^   50
      val x = ("x" | "X") ^^^   10
      val v = ("v" | "V") ^^^    5
      val i = ("i" | "I") ^^^    1

      def decade(decem: Parser[Int], quintum: Parser[Int], unit: Parser[Int]): Parser[Int] = (
        unit ~ decem          ^~  { (u, d)  => d - u       }
      | unit ~ quintum        ^~  { (u, q)  => q - u       }
      | quintum ~ unit.*(0,3) ^~  { (q, us) => q + us.sum  }
      | unit.*(1,3)           ^*  { (us)    => us.sum      }
      | ε                     ^^^ 0
      )

      decade(∅, ∅, m) ~ decade(m, d, c) ~ decade(c, l, x) ~ decade(x, v, i) <~ EOF ^~ { (a, b, c, d) => a + b + c + d }
    }
  }
}
