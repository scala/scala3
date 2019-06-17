object Test {
  enum Foo[X] {
    case Str extends Foo[String]
    case Int extends Foo[Int]
  }

  trait Test {
    type A

    def foo[T](f: Foo[A] | Foo[T]): T =
      f match { case Foo.Str =>
        "" // error
      }

    def bar[T](f: Unit | Foo[T]): T =
      f match { case Foo.Str =>
        ""
      }
  }
}
