object Test {
  extension (a: Int) {
    def foo(i: Int): Unit = ()
    def foo: Unit = ()
  }
  val x: Int = 5
  x.foo(4)
  x.foo
  Test.foo(x)(4)
  foo(x)
}