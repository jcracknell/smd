package object smd {
  implicit class UpgrayeddedCharSequence(val cs: CharSequence) extends IndexedSeq[Char] {
    def subSequenceProxy(start: Int, end: Int = cs.length()): CharSequence =
      new smd.util.ProxyCharSequence(cs, start, end)

    def literalEncode: String = util.LiteralEncoding.encode(cs)

    def length: Int = cs.length

    def apply(idx: Int): Char = cs.charAt(idx)
  }

  implicit class UpgrayeddedIterator[A](val it: Iterator[A]) extends AnyVal {
    def nextOption(): Option[A] = if(it.hasNext) Some(it.next()) else None
  }

  implicit class UpgrayeddedSeq[A](val seq: collection.Seq[A]) extends AnyVal {
    def lengthEq(len: Int): Boolean  = seq.lengthCompare(len) == 0
    def lengthGt(len: Int): Boolean  = seq.lengthCompare(len) >  0
    def lengthGte(len: Int): Boolean = seq.lengthCompare(len) >= 0
    def lengthLt(len: Int): Boolean  = seq.lengthCompare(len) <  0
    def lengthLte(len: Int): Boolean = seq.lengthCompare(len) <= 0
  }

  implicit class UpgrayeddedReader(val reader: java.io.Reader) {
    def bufferedReadAll(bufferSize: Int, dataHandler: (Array[Char], Int) => Unit): Unit = {
      val buffer = Array.ofDim[Char](bufferSize)
      do {
        val read = reader.read(buffer, 0, bufferSize)
        if(read < 0) return else dataHandler(buffer, read)
      } while(true)
    }
    def bufferedReadAll(dataHandler: (Array[Char], Int) => Unit): Unit = bufferedReadAll(4096, dataHandler)
  }

  //region using

  /** Use and safely dispose of the provided `resource`. */
  def using[A <% Disposable, B](resource: A)(act: A => B): B =
    try { act(resource) } finally { resource.dispose() }

  /** Trait describing a resource or object which requires safe disposal. */
  trait Disposable {
    def dispose(): Unit
  }

  object Disposable {
    implicit def javaAutoCloseable2Disposable(c: java.lang.AutoCloseable): Disposable =
      new Disposable { def dispose(): Unit = c.close() }

    implicit def javaCloseable2Disposable(c: java.io.Closeable): Disposable =
      new Disposable { def dispose(): Unit = c.close() }
  }

  //endregion
}
