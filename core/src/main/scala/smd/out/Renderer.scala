package smd
package out

import java.io.OutputStream
import smd.dom.Document

trait Renderer {
  def render(document: Document, ostream: OutputStream)
}