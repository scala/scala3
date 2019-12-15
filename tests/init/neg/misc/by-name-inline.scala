trait Foo {
  def next: Foo
}

object Foo {
  inline implicit def foo(implicit rec: => Foo): Foo =
    new Foo { def next = rec }
}

class A {
  val foo = implicitly[Foo]
  assert(foo eq foo.next)
}