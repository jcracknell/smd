package smd
package util

/** Trait explicitly encapsulating a lazily computed value. */
trait Lazy[+A] {
  /** Retrieves the lazily computed value of this instance, forcing evaluation. */
  def get: A

  /** Maps the value of this instance to a new instance without forcing evaluation. Evaluation
    * of the resulting instance will force evaluation. */
  def map[B](f: A => B): Lazy[B]
}

object Lazy {
  import scala.language.implicitConversions

  /** Creates a new [[smd.util.Lazy]] instance which lazily computes the provided expression. */
  implicit def apply[A](value: => A): Lazy[A] = new Lazy[A] {
    private var evaluated = false
    lazy val get: A = { evaluated = true; value }
    def map[B](f: A => B): Lazy[B] = Lazy(f(get))
    override def toString: String = if(evaluated) s"Lazy($value)" else "Lazy(?)"
  }

  /** Extractor for [[smd.util.Lazy]] instances. Pattern matching will force evaluation. */
  def unapply[A](lzy: Lazy[A]): Option[A] = Some(lzy.get)

  /** Wraps the provided function such that its results are lazily evaluated. */
  def mapping[A, B](f: A => B): A => Lazy[B] = ((a: A) => Lazy(f(a)))
}
