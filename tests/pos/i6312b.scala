class Foo {
  inline def foo: Unit = {
    @scala.annotation.compileTimeOnly("some message") val res = ???
    res
  }
}
