object Test {
  class Loop[T, U]
  object Loop {
    implicit def mkLoop[T, U](implicit tu: => Loop[T, U], ut: => Loop[U, T]): Loop[T, U] = ???
  }

  implicitly[Loop[Int, String]]
}
