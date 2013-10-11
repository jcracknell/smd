package smd
package grammar

import org.scalatest.{Matchers, FunSpec}

class NamedEntitySpec extends FunSpec with Matchers {
  Map(
    "eacute" -> Seq(233),
    "Aacute" -> Seq(193),      // first entry
    "zwnj"   -> Seq(8204),     // last entry
    "yfr"    -> Seq(120118),   // large value code point
    "acE"    -> Seq(8766, 819) // multiple code points
  ) foreach { case (name, codePoints) =>
    it(s"should define entity ${name.literalEncode} with code points ${codePoints.mkString("[",",","]")}") {
      val expected = NamedEntity(name, codePoints, codePoints.map(cp => new String(Character.toChars(cp))).mkString(""))
      NamedEntity.entities(name) should be (expected)
    }
  }
}
