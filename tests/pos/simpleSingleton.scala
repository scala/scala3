class Foo {
  def foo(x: Int) = {
    val a: x.type = x
    val b: Foo.type = Foo
    val c: Foo.Bar.type = Foo.Bar
  }
}

object Foo {
  object Bar
}
