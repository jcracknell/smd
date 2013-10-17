import sbt._
import java.io._
import java.net.{HttpURLConnection, URL}
import scala.annotation.tailrec
import spray.json.{JsNumber, JsArray, JsString, JsObject}

object NamedEntitiesGenerator {
  val sourceUri = new URL("http://www.w3.org/html/wg/drafts/html/master/entities.json")

  def generate(base: File): Unit = {
    val target = base/"smd"/"grammar"/"NamedEntities.bin"
    target.createNewFile()

    using(new FileOutputStream(target.getPath, false))(_.close()) { out =>
      using(new BufferedOutputStream(out))(_.close()) { out =>
        for((name, codepoints) <- getData) {
          // A full byte is needed - e.g. CapitalDifferentialD, and it doesn't really
          // matter as it should compress well anyways.
          out.write(name.length)
          for(c <- name)
            out.write(c.toByte)

          out.write(codepoints.length)
          for(cp <- codepoints) {
            out.write(cp >>> 24 & 0xFF)
            out.write(cp >>> 16 & 0xFF)
            out.write(cp >>>  8 & 0xFF)
            out.write(cp        & 0xFF)
          }
        }
        out.flush()
      }
    }
  }

  def getData(): Seq[(String, Seq[Int])]  =
    spray.json.JsonParser(getJson()).asJsObject.fields
    // The entity data contains some duplicate entries omitting the semicolon
    .filterKeys(_.endsWith(";"))
    .collect { case (escape, info @ JsObject(_)) =>
      info.getFields("characters", "codepoints") match {
        case Seq(JsString(chars), JsArray(cps)) =>
          val name = escape.substring(1, escape.length - 1)
          (name, cps collect { case JsNumber(v) => v.toIntExact })
        case _ => throw new Exception("Unexpected format")
      }
    }
    .toSeq
    .sortBy(_._1)

  def getJson(): String = {
    val connection = sourceUri.openConnection().asInstanceOf[HttpURLConnection]
    connection.setRequestMethod("GET")

    using(connection.getInputStream)(_.close()) { istream =>
      using(new InputStreamReader(istream, "utf-8"))(_.close()) { reader =>
        using(new BufferedReader(reader))(_.close()) { reader =>
          val buffer = Array.ofDim[Char](4096)
          @tailrec def readAll(sb: StringBuilder): String = {
            val n = reader.read(buffer, 0, buffer.length)
            if(n < 0) sb.toString() else readAll(sb.appendAll(buffer, 0, n))
          }

          readAll(new StringBuilder)
        }
      }
    }
  }

  def using[A, B](res: A)(cleanup: A => Unit)(act: A => B): B =
    try { act(res) } finally { cleanup(res) }
}
