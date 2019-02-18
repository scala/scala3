object Test extends App {
  class Bar[T]

  implicit def barInt: Bar[Int] = {
    println("barInt")
    new Bar[Int]
  }
  implicit def bar[T]: Bar[T] = {
    println("bar")
    new Bar[T]
  }

  implicitly[Bar[Int]]

  locally {
    def barInt: Unit = ???

    implicitly[Bar[Int]]
      // used to resolve to bar, but
      // resolves to barInt now, since shadowing is no longer tested
  }
}