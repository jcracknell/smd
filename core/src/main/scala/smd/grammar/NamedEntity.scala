package smd
package grammar

import java.io.BufferedInputStream

sealed case class NamedEntity(name: String, codePoints: Seq[Int], chars: String)

/** Facility providing a listing of all HTML5 named entities as defined by the W3C.
  * Loads data from the official entity list available from http://www.w3.org/html/wg/drafts/html/master/entities.json
  */
object NamedEntity {
  val entities: Map[String, NamedEntity] = {
    @inline def readCodepoint(in: BufferedInputStream): Int =
      in.read() << 24 | in.read() << 16 | in.read() << 8 | in.read()

    @inline def readNamedEntities(in: BufferedInputStream) = unfold {
      val nameLength = in.read()
      Option.when(nameLength >= 0) {
        val name = (nameLength :/ { n => Option.when(n > 0) { (in.read().toChar, n - 1) } })
                   .foldLeft(new StringBuilder)((sb, c) => sb.append(c))
                   .toString
        val codePoints = (in.read() :/ { n => Option.when(n > 0) { (readCodepoint(in), n - 1)} })
                         .toArray
        val str = ((new StringBuilder /: codePoints) { (sb, cp) => sb.appendAll(Character.toChars(cp)) })
                  .toString

        NamedEntity(name, codePoints, str)
      }
    }

    using(getClass.getClassLoader.getResourceAsStream("smd/grammar/NamedEntities.bin")) { in =>
      using(new BufferedInputStream(in)) { in =>
        readNamedEntities(in).map(e => (e.name, e)).toMap
      }
    }
  }
}
