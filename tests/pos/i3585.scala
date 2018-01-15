trait Foo[T]

object Foo {
  implicit def pair[T, U]
    (implicit
      fooT: => Foo[(T, U)],
      fooU: => Foo[(U, T)]
    ): Foo[(T, U)] = ???

  implicit def int: Foo[Int] = ???
  implicit def string: Foo[String] = ???
}

object Test extends App {
  implicitly[Foo[(Int, String)]]
}
