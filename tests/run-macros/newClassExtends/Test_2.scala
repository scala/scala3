@main def Test: Unit = {
  val foo: Foo = makeClass("foo")
  foo.foo()
  println(foo.getClass)
  val bar: Foo = makeClass("bar")
  bar.foo()
  println(bar.getClass)
}
