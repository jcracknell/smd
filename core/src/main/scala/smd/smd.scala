import scala.annotation.tailrec

package object smd {
  val Lazy = smd.util.Lazy
  type Lazy[A] = smd.util.Lazy[A]

  implicit class Unfoldable[A](val self: A) {
    @inline private def unfoldRight[B](seed: A, unspool: A => Option[(B, A)]): Stream[B] =
      unspool(seed) match {
        case Some((elem, sprout)) => elem #:: unfoldRight(sprout, unspool)
        case None                 => Stream.Empty
      }

    /** Generate a stream of values from the initial seed value. Alias for `unfoldRight`.
      *
      * @param unspool operation generating an output value from the current seed value. Returning `None` halts
      *                the unfolding process.
      * @tparam B element type of the resulting [[scala.collection.immutable.Stream]].
      */
    def :/[B](unspool: A => Option[(B, A)]): Stream[B] = unfoldRight(self, unspool)

    /** Generate a stream of values from the initial seed value. Alias for `:/`.
      *
      * @param unspool operation generating an output value from the current seed value. Returning `None` halts
      *                the unfolding process.
      * @tparam B element type of the resulting [[scala.collection.immutable.Stream]].
      */
    def unfoldRight[B](unspool: A => Option[(B, A)]): Stream[B] = unfoldRight(self, unspool)
  }

  implicit class UpgrayeddedCharSequence(val cs: CharSequence) extends IndexedSeq[Char] {
    def apply(idx: Int): Char = cs.charAt(idx)
    def length: Int = cs.length

    def literalEncode: String = util.LiteralEncoding.encode(cs)
    def repeat(n: Int): String = (1 to n).map(_ => cs).mkString
    def subSequenceProxy(start: Int, end: Int = cs.length()): CharSequence =
      new smd.util.ProxyCharSequence(cs, start, end)
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

  implicit class UpgrayeddedStreamCompanion(val self: Stream.type) extends AnyVal {
    def options[A](unspool: => Option[A]): Stream[A] = unspool match {
      case Some(elem) => elem #:: options(unspool)
      case None       => Stream.empty
    }
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

  /** Use and safely dispose of the provided `resource`. */
  def using[A <% smd.util.Disposable, B](resource: A)(act: A => B): B =
    try { act(resource) } finally { resource.dispose() }
}
