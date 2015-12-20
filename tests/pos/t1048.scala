trait T[U] {
  def x: T[_ <: U]
}

object T {
  def unapply[U](t: T[U]): Option[T[_ <: U]] = Some(t.x)
}

object Test {
  def f[W](t: T[W]) = t match {
    case T(T(_)) => ()
  }
}

