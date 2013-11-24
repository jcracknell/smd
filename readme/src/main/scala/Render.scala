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

  val transformer = new smd.out.html.HtmlTransformer()
  using(new FileOutputStream("./README.htm")) { ostream =>
    using(new BufferedOutputStream(ostream)) { ostream =>
      using(new OutputStreamWriter(ostream, "UTF-8")) { writer =>
        val renderer = new smd.out.html.HtmlRenderer(writer)
        for(block <- document.content) {
          val html = block.accept(transformer)
          html.accept(renderer)
        }

        writer.flush()
      }
    }
  }
}
