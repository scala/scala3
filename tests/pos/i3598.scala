class Foo[A] {
  def baz(foo: Foo[?]): Unit = bar(foo)
  def bam(foo: => Foo[?]) = bar(foo)
  def bar[A](foo: Foo[A]): A = ???
}
