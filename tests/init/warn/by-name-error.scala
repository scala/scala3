trait Foo {
  def next: Foo
}

object Foo {
  implicit def foo(implicit rec: => Foo): Foo =
    new Foo { def next = rec }
}

class A {
  val foo = implicitly[Foo]     // warn
  assert(foo eq foo.next)
}