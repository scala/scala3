import scala.language.experimental.erasedDefinitions

class Foo {
  def foo(f: (erased Int) => Int): Int = { // error: erased function parameters must be named
    erased val ctx = 1
    f(ctx)
  }
}
