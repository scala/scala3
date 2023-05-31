import pkg._

object Test {
  implicitly[Foo2[Qux]]
  implicitly[Foo1[Qux]]
}