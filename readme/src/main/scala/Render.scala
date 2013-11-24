package smd

import java.io._

object Render extends App {
  val sb = new StringBuilder
  using(new FileInputStream("./README.smd")) { istream =>
    using(new InputStreamReader(istream, "UTF-8")) { reader =>
      using(new BufferedReader(reader)) { reader =>
        reader.bufferedReadAll((buf, n) => sb.appendAll(buf, 0, n))
      }
    }
  }

  val input = sb.toString
  val document = smd.grammar.Grammar.document.parse(input).product

  val renderer = new smd.out.html.HtmlRenderer
  using(new FileOutputStream("./README.htm")) { ostream =>
    renderer.render(document, ostream)
  }
}
