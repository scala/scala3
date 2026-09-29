import scala.language.experimental.erasedDefinitions

class Foo {
  def foo(f: (erased x: Int) ?=> Int): Int = {
    implicit erased val ctx = 1
    f
  }
}
