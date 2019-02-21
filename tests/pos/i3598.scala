class Foo[A] {
  def baz(foo: Foo[_]): Unit = bar(foo)
  def bam(foo: => Foo[_]) = bar(foo)
  def bar[A](foo: Foo[A]): A = ???
}
