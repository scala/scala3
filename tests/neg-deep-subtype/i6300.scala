object Test {
  class Foo[T <: Foo[T]]
  class Bar extends Foo[Bar]

  implicit def i[T <: Foo[T]](implicit t: Foo[Foo[T]]): Foo[T] = ???

  implicitly[Foo[Bar]] // error
}
