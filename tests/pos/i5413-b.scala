object Foo {
  type Or[A]
  def foo[A](from: A => Or[A]): Any = null
  def id[A]: A => A = identity
  foo(id)
}
