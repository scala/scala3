trait T[U] {
  def x: T[_ <: U]
}

object T {
  def unapply[U](t: T[U]): Option[T[_ <: U]] = Some(t.x)
}

object Test {
  def f[W](t: T[W]) = t match {
    case T(T(_)) => ()
// Gives:
// t1048.scala:11: error: There is no best instantiation of pattern type T[Any']
// that makes it a subtype of selector type T[_ <: W].
// Non-variant type variable U cannot be uniquely instantiated.
//     case T(T(_)) => ()
//             ^
// one error found
  }
}

