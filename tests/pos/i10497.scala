trait Foo[+A] {
  def foo[B](implicit ev: A <:< Foo[B], s: Int = 0) = ???

  def bar(a: Foo[Foo[Int]]) = a.foo
}
