object Test {
  class Z
  class O[T]
  class E[T]

  class Expand[T, U]
  object Expand {
    implicit def expando[T]: Expand[O[T], E[O[T]]] = ???
    implicit def expande[T]: Expand[E[T], O[E[T]]] = ???
  }

  implicit def mkN[T, U](implicit e: => Expand[O[T], U], u: => U): O[T] = ???
  implicit def mkN[T, U](implicit e: => Expand[E[T], U], u: => U): E[T] = ???

  implicitly[O[Z]] // error
}
