import scala.annotation.tailrec

package object smd {
  implicit class UpgrayeddedAny[A](val self: A) extends UnfoldOps[A] {
    protected def repr: A = self
  }

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

  implicit class UpgrayeddedOptionCompanion(val self: Option.type) extends AnyVal {
    /** An Option factory which creates Some(value) in the event that the provided condition holds.
      *
      * @param cond the condition determining the resulting option.
      * @param value the value of the resulting option in the event that the provided condition holds.
      * @tparam A the type of the resulting Option.
      */
    def when[A](cond: Boolean)(value: => A): Option[A] = if(cond) Some(value) else None
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

  //region unfold

  /** Generate a [[scala.collection.immutable.Stream]] of values from an initial seed value.
    *
    * @param seed the initial seed value.
    * @param unspool operation generating an element to appear in the resulting stream. Returning `None`
    *                halts the unfolding process.
    * @tparam A the seed type.
    * @tparam B the element type of the output [[scala.collection.immutable.Stream]].
    */
  def unfoldRight[A, B](seed: A)(unspool: A => Option[(B, A)]): Stream[B] =
    unspool(seed) match {
      case Some((elem, sprout)) => elem #:: unfoldRight(sprout)(unspool)
      case None                 => Stream.empty[B]
    }

  trait UnfoldOps[A] {
    protected def repr: A

    /** Generate a stream of values from the initial seed value. Alias for `unfoldRight`.
      *
      * @param unspool operation generating an output value from the current seed value. Returning `None` halts
      *                the unfolding process.
      * @tparam B element type of the resulting [[scala.collection.immutable.Stream]].
      */
    def :/[B](unspool: A => Option[(B, A)]): Stream[B] = smd.unfoldRight(repr)(unspool)

    /** Generate a stream of values from the initial seed value. Alias for `:/`.
      *
      * @param unspool operation generating an output value from the current seed value. Returning `None` halts
      *                the unfolding process.
      * @tparam B element type of the resulting [[scala.collection.immutable.Stream]].
      */
    def unfoldRight[B](unspool: A => Option[(B, A)]): Stream[B] = smd.unfoldRight(repr)(unspool)
  }

  //endregion

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
