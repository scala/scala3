class Foo {
  inline def foo: Unit = {
    @scala.annotation.compileTimeOnly("some message") val res = ???
    res
  }

  inline def bar: Unit = {
    foo
  }

  erased def baz: Unit = {
    foo
  }
}
