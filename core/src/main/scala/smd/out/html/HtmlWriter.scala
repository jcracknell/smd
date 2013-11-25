package smd
package out
package html

import java.io.Writer

trait HtmlWriter { writer =>
  def writeBlock(tag: String, attrs: HtmlAttribute*)(content: => Unit): Unit
  def writeSpan(tag: String, attrs: HtmlAttribute*)(content: => Unit): Unit
  def writePre(tag: String, attrs: HtmlAttribute*)(content: => Unit): Unit
  def writeEmpty(tag: String, attrs: HtmlAttribute*): Unit
  def writeComment(text: HtmlString): Unit
  def writeText(text: HtmlString): Unit
  def writeRaw(text: String): Unit

  /** Object defining implicit conversions enabling use of the [[smd.out.html.HtmlFu]] API targeting this
    * [[smd.out.html.HtmlWriter]]. */
  object implicitApi {
    import scala.language.implicitConversions

    implicit def stringHtmlFu(tag: String): HtmlFu = new HtmlFu(writer, tag)
    implicit def symbolHtmlFu(sym: Symbol): HtmlFu = new HtmlFu(writer, sym.name)
  }
}

object HtmlWriter {
  val defaultConfiguration: Configuration = new Configuration {}

  trait Configuration {
    def openTagBegin: String  = "<"
    def openTagEnd: String    = ">"
    def closeTagBegin: String = "</"
    def closeTagEnd: String   = ">"
    def emptyTagBegin: String = "<"
    def emptyTagEnd: String   = " />"
    def commentBegin: String  = "<!-- "
    def commentEnd: String    = " -->"

    /** Formats an [[smd.out.html.HtmlAttribute.Boolean]] as it should be rendered by the [[smd.out.html.HtmlWriter]]. */
    def formatBooleanAttribute(attr: HtmlAttribute.Boolean): String = s"""${attr.name}="${attr.name}""""

    def baseIndent: String = ""
    /** The character sequence to be used as an indent by the [[smd.out.html.HtmlWriter]]. */
    def indent: String = "  "
    /** The newline sequence to be used by the [[smd.out.html.HtmlWriter]]. */
    def newline: String = System.lineSeparator()
  }
}

class StandardHtmlWriter(writer: Writer, conf: HtmlWriter.Configuration) extends HtmlWriter {
  private var _level: Int = 0

  def writeBlock(tag: String, attrs: HtmlAttribute*)(content: => Unit): Unit = {
    writeOpenTag(tag, attrs)
    _level += 1
    writeIndent()
    content
    _level -= 1
    writeIndent()
    writeCloseTag(tag)
    writeIndent()
  }

  def writeSpan(tag: String, attrs: HtmlAttribute*)(content: => Unit): Unit = {
    writeOpenTag(tag, attrs)
    content
    writeCloseTag(tag)
  }

  def writePre(tag: String, attrs: HtmlAttribute*)(content: => Unit): Unit = writeSpan(tag, attrs: _*)(content)

  def writeEmpty(tag: String, attrs: HtmlAttribute*): Unit = {
    writeEmptyTag(tag, attrs)
  }

  def writeComment(text: HtmlString): Unit = {
    writer.write(conf.commentBegin)
    writer.write(text.value)
    writer.write(conf.commentEnd)
  }

  def writeText(text: HtmlString): Unit =  writer.write(text.value)

  def writeRaw(text: String): Unit = writer.write(text)

  protected def level(content: => Unit): Unit = {
    _level += 1
    content
    _level -= 1
  }

  protected def writeOpenTag(tag: String, attrs: Seq[HtmlAttribute]): Unit = {
    writer.write(conf.openTagBegin)
    writer.write(tag)
    writeAttrs(attrs)
    writer.write(conf.openTagEnd)
  }

  protected def writeCloseTag(tag: String): Unit = {
    writer.write(conf.closeTagBegin)
    writer.write(tag)
    writer.write(conf.closeTagEnd)
  }

  protected def writeEmptyTag(tag: String, attrs: Seq[HtmlAttribute]): Unit = {
    writer.write(conf.emptyTagBegin)
    writer.write(tag)
    writeAttrs(attrs)
    writer.write(conf.emptyTagEnd)
  }

  protected def writeAttrs(attrs: Seq[HtmlAttribute]): Unit =
    attrs foreach {
      case HtmlAttribute.Value(name, HtmlString(value)) =>
        writer.write(' ')
        writer.write(name)
        writer.write("=\"")
        writer.write(value)
        writer.write('"')
      case attr: HtmlAttribute.Boolean =>
        writer.write(conf.formatBooleanAttribute(attr))
    }

  protected def writeIndent(): Unit = {
    writer.write(conf.newline)
    writer.write(conf.baseIndent)
    for(i <- 0 until _level)
      writer.write(conf.indent)
  }
}
