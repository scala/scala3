class Foo[T]
object Foo {
  implicit def intFoo: Foo[Int] = ???
}

object Test {
  implicit def genFoo[T](implicit f: Foo[T => T]): Foo[T] = ???

  def test: Unit = {
    implicitly[Foo[Int]]
  }
}
