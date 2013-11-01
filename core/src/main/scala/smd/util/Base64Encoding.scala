package smd
package util

import java.io.InputStream
import scala.annotation.tailrec

object Base64Encoding {
  private val Alphabet = Array(
    'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H',
    'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P',
    'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X',
    'Y', 'Z', 'a', 'b', 'c', 'd', 'e', 'f',
    'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n',
    'o', 'p', 'q', 'r', 's', 't', 'u', 'v',
    'w', 'x', 'y', 'z', '0', '1', '2', '3',
    '4', '5', '6', '7', '8', '9', '+', '/'
  )

  @tailrec private def encodeImpl(in: InputStream, out: Appendable): Unit = {
    @inline def e0(b0: Int): Char          = Alphabet((          b0 >>> 2)       )
    @inline def e1(b0: Int, b1: Int): Char = Alphabet((b0 << 4 | b1 >>> 4) & 0x3F) 
    @inline def e2(b1: Int, b2: Int): Char = Alphabet((b1 << 2 | b2 >>> 6) & 0x3F)
    @inline def e3(b2: Int): Char          = Alphabet((          b2      ) & 0x3F)

    val b0 = in.read()
    if(-1 != b0) {
      out.append(e0(b0))
      val b1 = in.read()
      if(-1 != b1) {
        out.append(e1(b0, b1))
        val b2 = in.read()
        if(-1 != b2) {
          out.append(e2(b1, b2))
          out.append(e3(b2))
          encodeImpl(in, out)
        } else { // b2 == -1
          out.append(e2(b1, 0))
          out.append('=')
        }
      } else { // b1 == -1
        out.append(e1(b0, 0))
        out.append('=')
        out.append('=')
      }
    }
  }

  def encode(in: InputStream, out: Appendable): Unit = encodeImpl(in, out)

  def encode(in: InputStream): String = {
    val writer = new java.io.StringWriter()
    encode(in, writer)
    writer.toString
  }

  def encode(data: Array[Byte]): String = {
    val sw = new java.io.StringWriter(data.length * 4 / 3 + 1)
    encode(new java.io.ByteArrayInputStream(data), sw)
    sw.toString
  }


  def encode(data: Array[Byte], out: Appendable): Unit =
    encode(new java.io.ByteArrayInputStream(data), out)
}
