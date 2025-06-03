class Foo {
  val a = Foo.bar(this)   // warn
  val b = "hello"
}

object Foo {
  def bar(foo: Foo) = foo.b + ", world"
}
