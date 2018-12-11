class Hello {
  class Foo {
    class Bar
    final lazy val s: Bar = ???
  }

  lazy val foo: Foo = ???

  val x: foo.s.type = ???   // error: `foo` must be final since it's a lazy val
  val x2: foo.s.type = ???  // error: `foo` must be final since it's a lazy val
}

