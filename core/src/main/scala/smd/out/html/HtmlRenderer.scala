package smd
package out
package html

import java.io.Writer

class HtmlRenderer(writer: Writer) extends HtmlNode.Visitor[Unit] {
  private def writeAttrs(attrs: Seq[HtmlAttribute]): Unit =
    for(attr <- attrs) attr match {
      case HtmlAttribute.Value(name, HtmlString(value)) => writer.write(s""" ${name}="${value}"""")
      case HtmlAttribute.Boolean(name)      => writer.write(s" $name")
    }

  def visit(node: HtmlNode.Comment): Unit = writer.write(s"<!-- ${node.text.value} -->")

  def visit(node: HtmlNode.Text): Unit = writer.write(node.text.value)

  def visit(node: HtmlNode.Empty): Unit = {
    writer.write(s"<${node.tag}")
    writeAttrs(node.attributes)
    writer.write(">")
  }

  def visit(node: HtmlNode.Block): Unit = {
    writer.write(s"<${node.tag}")
    writeAttrs(node.attributes)
    writer.write(">")
    node.children foreach { _.accept(this) }
    writer.write(s"</${node.tag}>")
  }

  def visit(node: HtmlNode.Inline): Unit = {
    writer.write(s"<${node.tag}")
    writeAttrs(node.attributes)
    writer.write(">")
    node.children foreach { _.accept(this) }
    writer.write(s"</${node.tag}>")
  }

  def visit(node: HtmlNode.Pre): Unit = {
    writer.write(s"<${node.tag}")
    writeAttrs(node.attributes)
    writer.write(">")
    node.children foreach { _.accept(this) }
    writer.write(s"</${node.tag}>")
  }
}
