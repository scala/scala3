// Paths rooted in outer classes. Not sure to what degree we
// want to support them.
class Outer {
  type Bar
  object inner {
    type Foo
    def foo: Foo = ???
  }
  def bar: Bar = ???
}

object Main {
  val a = new Outer
  val b = new Outer
  def all = List(a.inner.foo, a.inner.foo)
    // The inferred type is [Outer#inner.Foo], but this cannot be written in source
  def bars: Outer # Bar = identity(a.bar)
}