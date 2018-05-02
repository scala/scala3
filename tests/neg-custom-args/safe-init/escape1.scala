class Foo {
  val a = Foo.bar(this)   // error
  val b = "hello"
}

object Foo {
  def bar(foo: Foo) = foo.b + ", world"
}
