object Foo {
  inline def foo(i: => Int): Unit  = ???
  inline def foo(l: => Long): Unit = ???

  foo(2)
  foo(3L)
}
