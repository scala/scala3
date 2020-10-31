object Test {
  extension (a: Int) {
    def foo(i: Int): Unit = ()
    def foo: Unit = ()
  }
  val x: Int = 5
  x.foo(4)
  x.foo
  Test.extension_foo(x)(4)
  this.extension_foo(x)
}