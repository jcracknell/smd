package smd
package util

/** Adds support for the creation of [[scala.Option]] values using a `when`/`unless` postfix syntax. */
trait PostfixOptionImplicits {
  implicit class PostfixOption[A](a: => A) {
    /** Returns an [[scala.Option]] containing the current value if the provided condition is true. */
    def when(cond: Boolean): Option[A] = if(cond) Some(a) else None

    /** Returns an [[scala.Option]] containing the current value if the provided condition is false. */
    def unless(cond: Boolean): Option[A] = if(cond) None else Some(a)
  }
}
