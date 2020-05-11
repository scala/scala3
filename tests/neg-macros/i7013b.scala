import quoted._

class Foo {
  class Bar
  def foo(using s: Scope)() = {
    '[Bar] // error
  }
}
