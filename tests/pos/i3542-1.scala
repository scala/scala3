class Foo[T]

object Test {
  implicit def foo[T](implicit rec: => Foo[T]): Foo[T] = ???

  val bla: Foo[Int] = implicitly[Foo[Int]]
}
