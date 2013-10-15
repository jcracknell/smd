package smd
package grammar

import java.io.InputStreamReader
import scala.util.parsing.json.JSON

sealed case class NamedEntity(name: String, codePoints: Seq[Int], chars: String)

/** Facility providing a listing of all HTML5 named entities as defined by the W3C.
  * Loads data from the official entity list available from http://www.w3.org/html/wg/drafts/html/master/entities.json
  */
object NamedEntity {
  /** The complete set of named entity values by name. */
  val entities: Map[String, NamedEntity] = {
    val namedEntitiesResourceString: String = {
      val stream = getClass.getClassLoader.getResourceAsStream("smd/grammar/NamedEntity.json")
      try {
        val reader = new InputStreamReader(stream, "UTF-8")
        try {
          val sb = new StringBuilder()
          reader.bufferedReadAll { (buf, n) => sb.appendAll(buf, 0, n) }
          sb.toString()
        } finally { reader.close() }
      } finally { stream.close() }
    }

    // The entity resource file is JSON-formatted as follows:
    // { "&Aacute;": { "codepoints": [193], "characters": "\u00C1" }, ... }
    JSON.parseFull(namedEntitiesResourceString).get.asInstanceOf[Map[String, Map[String, Any]]]
    // Strip duplicate entries for entities where the trailing semicolon is optional (apparently?)
    .filterKeys(k => ';' == k.charAt(k.length - 1))
    .map({ case (entity, data) =>
      val entityName = entity.substring(1, entity.length - 1)
      entityName -> NamedEntity(
                      name = entityName,
                      codePoints = data("codepoints").asInstanceOf[Seq[Double]].map(_.toInt),
                      chars = data("characters").toString
                    )
    })
  }
}
