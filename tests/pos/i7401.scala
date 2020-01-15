object Test {
  extension ops on (a: Int) {
    def foo(i: Int): Unit = ()
    def foo: Unit = ()
  }
  val x: Int = 5
  x.foo(4)
  x.foo
  ops.foo(x)(4)
  ops.foo(x)
}