package smd
package util

/** Simple implementation of a prefix trie mapping character sequences to one or more values. */
case class Trie[+A](value: Option[A], subtries: Map[Char, Trie[A]]) extends Iterable[(String, A)] {

  def values(path: CharSequence): Seq[(String, A)] = values(path.iterator, "")

  protected def values(path: Iterator[Char], loc: String): Seq[(String, A)] =
    value.map(v => (loc, v)).toSeq ++ (path.nextOption() match {
      case Some(c) => subtrie(c).map(_.values(path, loc + c)).getOrElse(Seq())
      case _ => Seq()
    })

  def subtrie(c: Char): Option[Trie[A]] = subtries.get(c)

  def subtrie(path: CharSequence): Option[Trie[A]] = subtrie(path.iterator)

  protected def empty: Trie[A] = Trie.empty[A]

  protected def subtrie(it: Iterator[Char]): Option[Trie[A]] = it.nextOption() match {
    case Some(c) => subtrie(c).map(_.subtrie(it)).flatten
    case _ => None
  }

  protected def set[B >: A](v: B): Trie[B] = new Trie(Some(v), subtries)

  protected def attach[B >: A](c: Char, subtrie: Trie[B]): Trie[B] = new Trie(value, subtries + (c -> subtrie))

  def +[B >: A](entry: (CharSequence, B)): Trie[B] = add(entry._1.iterator, entry._2)

  def ++[B >: A](entries: Iterable[(CharSequence, B)]): Trie[B] =
    (this.asInstanceOf[Trie[B]] /: entries) { (t, e) => t + e }

  protected def add[B >: A](it: Iterator[Char], v: B): Trie[B] = it.nextOption() match {
    case Some(c) => attach(c, subtrie(c).getOrElse(empty).add(it, v))
    case None    => set(v)
  }

  def -(path: CharSequence): Trie[A] = ???

  def --(paths: Iterable[CharSequence]): Trie[A] = (this /: paths) { (t, p) => t - p }

  def rem(it: Iterator[Char]): Trie[A] = it.nextOption() match {
    case Some(c) => subtrie(c).map(_.rem(it)).getOrElse(this)
    case None => this
  }

  def iterator: Iterator[(String, A)] = iterator("")

  protected def iterator(path: String): Iterator[(String, A)] =
    value.map(v => (path, v)).iterator ++ subtries.iterator.flatMap({ case (c, sub) => sub.iterator(path + c) })
}

object Trie {
  private val seed = new Trie[Nothing](None, Map())

  def empty[A]: Trie[A] = seed.asInstanceOf[Trie[A]]

  def apply[A](): Trie[A] = empty[A]

  def apply[A](value: Option[A]): Trie[A] = value match {
    case Some(v) => new Trie[A](value, Map())
    case _ => empty[A]
  }

  def apply[A](e0: (CharSequence, A), entries: (CharSequence, A)*): Trie[A] = apply(e0 +: entries)

  def apply[A](entries: Iterable[(CharSequence, A)]): Trie[A] = (empty[A] /: entries) { (t, e) => t + e }
}

