class Foo {
  inline def foo: Unit = {
    scala.compiletime.error("some message")
  }
}
