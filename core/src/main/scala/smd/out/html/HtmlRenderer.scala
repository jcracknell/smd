package smd
package out
package html

import smd.dom._
import java.io.{BufferedOutputStream, OutputStream, OutputStreamWriter}
import java.nio.charset.Charset

class HtmlRenderer(
  configuration: HtmlWriter.Configuration = HtmlWriter.defaultConfiguration,
  charSet: Charset                        = Charset.forName("UTF-8")
) {

  def render(document: Document, ostream: OutputStream): Unit =
    using(new BufferedOutputStream(ostream)) { ostream =>
      using(new OutputStreamWriter(ostream, charSet)) { writer =>
        val htmlWriter = new StandardHtmlWriter(writer, configuration)
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
      'blockquote.block() {
        node.children foreach { _.accept(this) }
      }

    def visit(node: ExpressionBlock): Unit =
      'pre.pre('class -> "markdown-expression") {
        // TODO: Expression evaluation
        writer.writeText(node.expr.toString)
      }

    def visit(node: Heading): Unit =
      if(node.level <= 6)
        s"h${node.level}".block('id -> "") { renderNodes(node.children) }
      else
        'div.block("data-heading-level" -> node.level) { renderNodes(node.children) }

    def visit(node: Paragraph): Unit =
      'p.block()(node.children map { _.accept(this) })

    def visit(node: Reference): Unit = 'span.empty()

    def visit(node: DefinitionList.Loose): Unit =
      'dl.block('class -> "dl-loose") {
        node.items foreach { item =>
          'dt.block() { renderNodes(item.term.children) }
          item.defs foreach { definition =>
            'dd.block() { renderNodes(definition.children) }
          }
        }
      }

    def visit(node: DefinitionList.Tight): Unit =
      'dl.block('class -> "dl-tight") {
        node.items foreach { item =>
          'dt.block() { renderNodes(item.term.children) }
          item.defs foreach { definition =>
            'dd.block() { renderNodes(definition.children) }
          }
        }
      }

    def visit(node: OrderedList.Loose): Unit =
      'ol.block(
        'class -> s"ol-loose ol-${node.counter.numeralStyle.toString.toLowerCase} ol-${node.counter.separatorStyle.toString.toLowerCase}",
        node.counter.start map ("data-counter-start" -> _)
      ) {
        node.items foreach { item =>
          'li.block() { renderNodes(item.children) }
        }
      }

    def visit(node: OrderedList.Tight): Unit =
      'ol.block(
        'class -> s"ol-tight ol-${node.counter.numeralStyle.toString.toLowerCase} ol-${node.counter.separatorStyle.toString.toLowerCase}",
        node.counter.start map ("data-counter-start" -> _)
      ) {
        node.items foreach { item =>
          'li.block() { renderNodes(item.children) }
        }
      }

    def visit(node: Table): Unit =
      'table.block() {
        'thead.block() {
          node.head foreach { row =>
            'tr.block() {
              row.cells foreach { cell =>
                'th.block('colspan -> cell.span when cell.span > 1) { renderNodes(cell.children) }
              }
            }
          }
        }
        'tbody.block() {
          node.body foreach { row =>
            'tr.block() {
              row.cells foreach { cell =>
                'td.block('colspan -> cell.span when cell.span > 1) { renderNodes(cell.children) }
              }
            }
          }
        }
      }

    def visit(node: UnorderedList.Loose): Unit =
      'ul.block('class -> "ul-loose") {
        node.items map { item =>
          'li.block() { renderNodes(item.children) }
        }
      }

    def visit(node: UnorderedList.Tight): Unit =
      'ul.block('class -> "ul-tight") {
        node.items map { item =>
          'li.block() { renderNodes(item.children) }
        }
      }

    def visit(node: Emphasis): Unit =
      'em.inline() { renderNodes(node.children) }

    def visit(node: Link): Unit =
      'a.inline('href -> "") { renderNodes(node.children) }


    def visit(node: Quoted): Unit =
      'q.inline('class -> node.kind.toString.toLowerCase) { renderNodes(node.children) }

    def visit(node: Strong): Unit =
      'strong.inline() { renderNodes(node.children) }

    def visit(node: Subscript): Unit =
      'sub.inline() { renderNodes(node.children) }

    def visit(node: Superscript): Unit =
      'sup.inline() { renderNodes(node.children) }

    def visit(node: AutoLink): Unit =
      'a.inline('href -> node.uri) { writer.writeText(node.uri) }

    def visit(node: Code): Unit =
      'code.pre() { writer.writeText(node.value) }

    def visit(node: InlineExpression): Unit =
      'pre.pre('class -> "inline-expression") { writer.writeText(node.expr.toString) }

    def visit(node: LineBreak): Unit = 'br.empty()

    def visit(node: Space): Unit = writer.writeRaw(" ")

    def visit(node: Symbol): Unit = writer.writeText(node.value)

    def visit(node: Text): Unit = writer.writeText(node.value)

    def visit(node: Entity): Unit =
      node.codePoints foreach { cp => writer.writeRaw(s"&$cp;") }
  }
}
