package smd.out.html

import smd.dom._

class HtmlTransformer extends Markdown.Visitor[HtmlNode] {
  import smd.util.PostfixOptionImplicits._
  import HtmlFu._

  def visit(node: Blockquote): HtmlNode =
    'blockquote.block()(node.children map { _.accept(this) })

  def visit(node: ExpressionBlock): HtmlNode =
    'pre.pre('class -> "markdown-expression")(node.expr.toString)

  def visit(node: Heading): HtmlNode =
    if(node.level <= 6)
      s"h${node.level}".block('id -> "")(node.children map { _.accept(this) })
    else
      'div.block("data-heading-level" -> node.level)(node.children map { _.accept(this) })

  def visit(node: Paragraph): HtmlNode =
    'p.block()(node.children map { _.accept(this) })

  def visit(node: Reference): HtmlNode = 'span.empty()

  def visit(node: DefinitionList.Loose): HtmlNode =
    'dl.block('class -> "dl-loose")(
      node.items flatMap { item =>
        val t = 'dt.block()(item.term.children map { _.accept(this) })
        val ds = item.defs map { d => 'dd.block()(d.children map { _.accept(this) })}
        t +: ds
      }
    )

  def visit(node: DefinitionList.Tight): HtmlNode =
    'dl.block('class -> "dl-tight")(
      node.items flatMap { item =>
        val t = 'dt.block()(item.term.children map { _.accept(this) })
        val ds = item.defs map { d => 'dd.block()(d.children map { _.accept(this) })}
        t +: ds
      }
    )

  def visit(node: OrderedList.Loose): HtmlNode =
    'ol.block(
      'class -> s"ol-loose ol-${node.counter.numeralStyle.toString.toLowerCase} ol-${node.counter.separatorStyle.toString.toLowerCase}",
      node.counter.start map ("data-counter-start" -> _)
    )(
      node.items map { item =>
        'li.block()(item.children map { _.accept(this) })
      }
    )

  def visit(node: OrderedList.Tight): HtmlNode =
    'ol.block(
      'class -> s"ol-tight ol-${node.counter.numeralStyle.toString.toLowerCase} ol-${node.counter.separatorStyle.toString.toLowerCase}",
      node.counter.start map ("data-counter-start" -> _)
    )(
      node.items map { item =>
        'li.block()(item.children map { _.accept(this) })
      }
    )

  def visit(node: Table): HtmlNode =
    'table.block()(
      'thead.block() {
        node.head map { row =>
          'tr.block()(
            row.cells map { cell =>
              'th.block(
                'class   -> s"cell-align-${cell.alignment.toString.toLowerCase}",
                'colspan -> cell.span when cell.span > 1
              )(
                cell.children map { _.accept(this) }
              )
            }
          )
        }
      },
      'tbody.block() {
        node.head map { row =>
          'tr.block()(
            row.cells map { cell =>
              'th.block(
                'class   -> s"cell-align-${cell.alignment.toString.toLowerCase}",
                'colspan -> cell.span when cell.span > 1
              )(
                cell.children map { _.accept(this) }
              )
            }
          )
        }
      }
    )

  def visit(node: UnorderedList.Loose): HtmlNode =
    'ul.block('class -> "ul-loose")(
      node.items map { item => 'li.block()(item.children map { _.accept(this) }) }
    )

  def visit(node: UnorderedList.Tight): HtmlNode =
    'ul.block('class -> "ul-tight")(
      node.items map { item => 'li.block()(item.children map { _.accept(this) }) }
    )

  def visit(node: Emphasis): HtmlNode =
    'em.inline()(node.children map { _.accept(this) })

  def visit(node: Link): HtmlNode =
    'a.inline('href -> "")(node.children map { _.accept(this) })

  def visit(node: Quoted): HtmlNode =
    'q.inline('class -> node.kind.toString.toLowerCase)(node.children map { _.accept(this) })

  def visit(node: Strong): HtmlNode =
    'strong.inline()(node.children map { _.accept(this) })

  def visit(node: Subscript): HtmlNode =
    'sub.inline()(node.children map { _.accept(this) })

  def visit(node: Superscript): HtmlNode =
    'sup.inline()(node.children map { _.accept(this) })

  def visit(node: AutoLink): HtmlNode =
    'a.inline('href -> node.uri)(node.uri)

  def visit(node: Code): HtmlNode =
    'code.pre()(node.value)

  def visit(node: InlineExpression): HtmlNode =
    'pre.pre('class -> "inline-expression")(node.expr.toString)

  def visit(node: LineBreak): HtmlNode = 'br.empty()

  def visit(node: Space): HtmlNode = HtmlNode.Text(" ")

  def visit(node: Symbol): HtmlNode = HtmlNode.Text(node.value)

  def visit(node: Text): HtmlNode = HtmlNode.Text(node.value)

  def visit(node: Entity): HtmlNode = HtmlNode.Text(HtmlString.raw(node.codePoints.map(i => s"&#$i;").mkString))
}
