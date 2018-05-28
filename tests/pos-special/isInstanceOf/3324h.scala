object Test {
  trait Marker
  def foo[T](x: T) = x match {
    case _: (T & Marker)       => // no warning
    case _ =>
  }

  def foo2[T](x: T) = x match {
    case _: T with Marker      => // scalac emits a warning
    case _ =>
  }
}
