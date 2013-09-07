package object smd {
  protected trait BufferedReading[A] {
    def bufferedReadAll(bufferSize: Int, act: (Array[A], Int) => Unit): Unit
    def bufferedReadAll(act: (Array[A], Int) => Unit): Unit = bufferedReadAll(4096, act);
  }

  implicit class RichCharSequence(val cs: CharSequence) extends AnyVal {
    def proxySubSequence(start: Int, end: Int): CharSequence =
      new smd.util.ProxyCharSequence(cs, start, end)
  }

  implicit class RichSeq[A](val seq: collection.Seq[A]) extends AnyVal {
    def lengthEq(len: Int): Boolean  = seq.lengthCompare(len) == 0
    def lengthGt(len: Int): Boolean  = seq.lengthCompare(len) >  0
    def lengthGte(len: Int): Boolean = seq.lengthCompare(len) >= 0
    def lengthLt(len: Int): Boolean  = seq.lengthCompare(len) <  0
    def lengthLte(len: Int): Boolean = seq.lengthCompare(len) <= 0
  }

  implicit class RichString(val s: String) extends AnyVal {
    def literalEncode: String = util.LiteralEncoding.encode(s)
  }

  implicit class RichReader(val reader: java.io.Reader) extends BufferedReading[Char] {
    def bufferedReadAll(bufferSize: Int, act: (Array[Char], Int) => Unit): Unit = {
      val buffer = Array.ofDim[Char](bufferSize)
      do {
        val read = reader.read(buffer, 0, bufferSize)
        if(read < 0) return else act(buffer, read)
      } while(true)
    }
  }
}
