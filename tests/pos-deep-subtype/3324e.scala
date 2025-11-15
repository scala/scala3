//> using options -Werror

class C[T] {
  val x: T = ???
  x.isInstanceOf[T]

  val y: Array[T] = ???

  y match {
    case x: Array[T] =>
  }

  type F[X]

  val z: F[T] = ???
  z match {
    case x: F[T] =>
  }
}
