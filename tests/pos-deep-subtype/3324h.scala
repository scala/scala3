//> using options -Werror

import scala.language.`3.3`

object Test {
  trait Marker
  def foo[T](x: T) = x match {
    case _: (T & Marker)       => // no warning
    case _ =>
  }

  def foo2[T](x: T) = x match {
    case _: T with Marker      => // scalac or 3.4 emits a warning
    case _ =>
  }
}
