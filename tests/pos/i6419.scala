import scala.language.experimental.erasedDefinitions

class Foo {
  inline def foo: Unit = {
    @scala.annotation.compileTimeOnly("some message") val res = ???
    res
  }

  inline def bar: Unit = {
    foo
  }
}
