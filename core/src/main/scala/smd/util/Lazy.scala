package smd
package util

trait Lazy[+A] {
  def get: A
  def map[B](f: A => B): Lazy[B]
}

object Lazy {
  import scala.language.implicitConversions

  /** Creates a new [[smd.util.Lazy]] instance which lazily computes the provided expression. */
  implicit def apply[A](thunk: => A): Lazy[A] = new Lazy[A] {
    private var unevaluated = true
    private var value: A = _

    def get: A = {
      if(unevaluated) synchronized {
        if(unevaluated) {
          value = thunk
          unevaluated = false
        }
      }
      value
    }

    def map[B](f: A => B): Lazy[B] = Lazy(f(get))

    override def toString: String = if(unevaluated) "Lazy(???)" else s"Lazy($value)"
  }

  /** Wraps the provided function such that its results are lazily evaluated. */
  def mapping[A, B](f: A => B): A => Lazy[B] = ((a: A) => Lazy(f(a)))
}
