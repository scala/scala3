class Foo[A] {
  def baz(foo: Foo[_]): Unit = bar(foo)
  def bar[A](foo: Foo[A]): A = ???
}
