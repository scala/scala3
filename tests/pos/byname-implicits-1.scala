trait Foo
object Foo {
  implicit def foo(implicit rec: => Foo): Foo = ???

  //implicit def foo(implicit rec: => Int): Foo = ???
}

object Test {
  //implicit def i: Int = 23

  implicitly[Foo]
}
