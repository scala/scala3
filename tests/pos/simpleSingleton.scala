class Foo {
  def foo(x: Int) = {
    val a: x.type = x
    val b: Foo.type = Foo
  }
}

object Foo
