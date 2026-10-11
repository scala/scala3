import scala.language.experimental.erasedDefinitions

class Foo {
  def foo(f: (erased x: Int) => Int): Int = {
    erased val ctx = 1
    f(ctx)
  }
}
