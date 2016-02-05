class Foo{
  val default = this
  def foo(a: Int)(b: Foo = default): b.type = b

  def bar(b: Foo = default): b.type = b
  val x: Foo = bar() // ok
  val x2: Foo = foo(1)() // ok

  val s: Foo = foo(1) // error
  val s2: default.type = foo(1) // error
}
