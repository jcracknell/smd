package smd.util

/** Trait describing a resource or object which requires safe disposal. */
trait Disposable {
  def dispose(): Unit
}

object Disposable {
  import scala.language.implicitConversions

  implicit def javaAutoCloseable2Disposable(c: java.lang.AutoCloseable): Disposable =
    new Disposable { def dispose(): Unit = c.close() }

  implicit def javaCloseable2Disposable(c: java.io.Closeable): Disposable =
    new Disposable { def dispose(): Unit = c.close() }
}

