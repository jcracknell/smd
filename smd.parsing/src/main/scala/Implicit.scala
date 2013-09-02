package smd

/** Encapsulates the result of an implicit conversion, allowing the result type of the conversion to be used for
  * type inferral in an implicit parameter list. */
trait Implicit[+A] {
  /** Retrieve the implicit value. */
  def get: A
}

object Implicit {
  /** Create an [[smd.Implicit]] with the provided value. */
  implicit def apply[A](value: A): Implicit[A] =
    new Implicit[A] { def get: A = value }

  implicit def unapply[A](imp: Implicit[A]): Option[(A)] = Some((imp.get))

  /** Implicit conversion unpacking an [[smd.Implicit]] into its stored value. */
  implicit def unpack[A](imp: Implicit[A]): A = imp.get
}
