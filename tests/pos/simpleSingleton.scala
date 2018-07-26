class Foo {
  def foo(x: Int) = {
    val a: x.type = x
    val b: Foo.type = Foo
    val c: Foo.Bar.type = Foo.Bar
    val d: 1 = 1
    val e: "abc" = "abc"
  }
}

object Foo {
  object Bar
}
