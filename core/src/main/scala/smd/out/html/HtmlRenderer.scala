package smd
package out
package html

import smd.dom._
import java.io.{BufferedOutputStream, OutputStream, OutputStreamWriter}
import java.nio.charset.Charset

class HtmlRenderer(
  configuration: HtmlRenderer.Configuration = HtmlRenderer.defaultConfiguration,
  charSet: Charset = Charset.forName("UTF-8"),
  htmlWriterConfiguration: HtmlWriter.Configuration = HtmlWriter.defaultConfiguration
) extends Renderer {

  def render(document: Document, ostream: OutputStream): Unit =
    using(new BufferedOutputStream(ostream)) { ostream =>
      using(new OutputStreamWriter(ostream, charSet)) { writer =>
        val htmlWriter = new StandardHtmlWriter(writer, htmlWriterConfiguration)
        val visitor = new RenderingVisitor(htmlWriter)

        document.content foreach { _.accept(visitor) }

        writer.flush()
      }
    }

  class RenderingVisitor(writer: HtmlWriter) extends Markdown.Visitor[Unit] {
    import smd.util.PostfixOptionImplicits._
    import writer.implicitApi._

    protected def renderNodes(nodes: Seq[Markdown]): Unit = nodes foreach { _.accept(this) }

    def visit(node: Blockquote): Unit =
      'blockquote.block {
        node.content foreach { _.accept(this) }
      }

    def visit(node: ExpressionBlock): Unit =
      'div.block('class -> s"${configuration.cssNamespace}expression-block") {
        'pre.pre('class -> s"${configuration.cssNamespace}markdown-expression") {
          // TODO: Expression evaluation
          writer.writeText(node.expr.toString)
        }
      }

    def visit(node: Heading): Unit =
      (if(node.level <= 6) s"h${node.level}" else "div").block(
        'id -> "",
        'class -> s"${configuration.cssNamespace}heading",
        s"data-${configuration.attrNamespace}heading-level" -> node.level
      ) { renderNodes(node.content) }

    def visit(node: Paragraph): Unit =
      'p.block { renderNodes(node.content) }

    def visit(node: Reference): Unit = { }

    def visit(node: LooseDefinitionList): Unit =
      'dl.block('class -> s"${configuration.cssNamespace}dl-loose") {
        node.items foreach { item =>
          'dt.block('class -> s"${configuration.cssNamespace}dl-loose-term") { renderNodes(item.term.content) }
          item.defs foreach { definition =>
            'dd.block('class -> s"${configuration.cssNamespace}dl-loose-def") { renderNodes(definition.content) }
          }
        }
      }

    def visit(node: TightDefinitionList): Unit =
      'dl.block('class -> s"${configuration.cssNamespace}dl-tight") {
        node.items foreach { item =>
          'dt.block('class -> s"${configuration.cssNamespace}dl-tight-term") { renderNodes(item.term.content) }
          item.defs foreach { definition =>
            'dd.block('class -> s"${configuration.cssNamespace}dl-tight-def") {
              if(!definition.content.isEmpty)
                'div.block('class -> s"${configuration.cssNamespace}dl-tight-def-content") { renderNodes(definition.content) }
              renderNodes(definition.sublists)
            }
          }
        }
      }

    def visit(node: LooseOrderedList): Unit =
      'ol.block(
        'class -> s"${configuration.cssNamespace}ol-loose ${configuration.cssNamespace}ol-${node.counter.numeralStyle.toString.toLowerCase} ${configuration.cssNamespace}ol-${node.counter.separatorStyle.toString.toLowerCase}",
        node.counter.start map (s"data-${configuration.attrNamespace}counter-start" -> _)
      ) {
        node.items foreach { item =>
          'li.block('class -> s"${configuration.cssNamespace}ol-loose-item") { renderNodes(item.content) }
        }
      }

    def visit(node: TightOrderedList): Unit =
      'ol.block(
        'class -> s"${configuration.cssNamespace}ol-tight ${configuration.cssNamespace}ol-${node.counter.numeralStyle.toString.toLowerCase} ${configuration.cssNamespace}ol-${node.counter.separatorStyle.toString.toLowerCase}",
        node.counter.start map (s"data-${configuration.attrNamespace}counter-start" -> _)
      ) {
        node.items foreach { item =>
          'li.block('class -> s"${configuration.cssNamespace}ol-tight-item") {
            if(!item.content.isEmpty)
              'div.block('class -> s"${configuration.cssNamespace}ol-tight-content") { renderNodes(item.content) }
            renderNodes(item.sublists)
          }
        }
      }

    def visit(node: Table): Unit =
      'table.block {
        'thead.block {
          node.head foreach { row =>
            'tr.block {
              row.cells foreach { cell =>
                'th.block('colspan -> cell.span when cell.span > 1) { renderNodes(cell.content) }
              }
            }
          }
        }
        'tbody.block {
          node.body foreach { row =>
            'tr.block {
              row.cells foreach { cell =>
                'td.block('colspan -> cell.span when cell.span > 1) { renderNodes(cell.content) }
              }
            }
          }
        }
      }

    def visit(node: LooseUnorderedList): Unit =
      'ul.block('class -> s"${configuration.cssNamespace}ul-loose") {
        node.items map { item =>
          'li.block('class -> s"${configuration.cssNamespace}ul-loose-item") { renderNodes(item.content) }
        }
      }

    def visit(node: TightUnorderedList): Unit =
      'ul.block('class -> s"${configuration.cssNamespace}ul-tight") {
        node.items map { item =>
          'li.block('class -> s"${configuration.cssNamespace}ul-tight-item") {
            if(!item.content.isEmpty)
              'div.block('class -> s"${configuration.cssNamespace}ul-tight-content") { renderNodes(item.content) }
            renderNodes(item.sublists)
          }
        }
      }

    def visit(node: Attributes): Unit = { }

    def visit(node: Emphasis): Unit =
      'em.inline { renderNodes(node.content) }

    def visit(node: Link): Unit =
      'a.inline('href -> "") { renderNodes(node.content) }


    def visit(node: Quoted): Unit =
      'q.inline('class -> node.kind.toString.toLowerCase) { renderNodes(node.content) }

    def visit(node: Strong): Unit =
      'strong.inline { renderNodes(node.content) }

    def visit(node: Subscript): Unit =
      'sub.inline { renderNodes(node.content) }

    def visit(node: Superscript): Unit =
      'sup.inline { renderNodes(node.content) }

    def visit(node: AutoLink): Unit =
      'a.inline('href -> node.uri) { writer.writeText(node.uri) }

    def visit(node: Code): Unit =
      'code.pre { writer.writeText(node.value) }

    def visit(node: InlineExpression): Unit =
      'pre.pre('class -> s"${configuration.cssNamespace}inline-expression") { writer.writeText(node.expr.toString) }

    def visit(node: LineBreak): Unit = 'br.empty

    def visit(node: Space): Unit = writer.writeRaw(" ")

    def visit(node: Symbol): Unit = writer.writeText(node.value)

    def visit(node: Text): Unit = writer.writeText(node.value)

    def visit(node: Entity): Unit =
      node.codePoints foreach { cp => writer.writeRaw(s"&#$cp;") }
  }
}

object HtmlRenderer {
  val defaultConfiguration: Configuration = new Configuration { }

  trait Configuration {
    def cssNamespace: String = "smd-"
    def attrNamespace: String = "smd-"
  }
}
