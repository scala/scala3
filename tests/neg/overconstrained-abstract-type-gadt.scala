trait Test {
  type A

  enum Foo[X, Y] {
    case StrStr() extends Foo[String, String]
    case IntInt() extends Foo[Int, Int]
  }

  def foo[T, U](f: Foo[A, T] | Foo[String, U]): Unit =
    f match { case Foo.StrStr() =>
      val t: T = "" // error
      val u: U = "" // error
    }
}
