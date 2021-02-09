trait Test {
  def q: Quux
  def f: Foo
  q.baz[Foo](f)
}
