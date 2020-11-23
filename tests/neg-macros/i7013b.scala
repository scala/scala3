import quoted._

class Foo {
  class Bar
  def foo()(using Quotes) = {
    Type[Bar] // error
  }
}
