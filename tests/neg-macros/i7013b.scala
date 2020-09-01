import quoted._

class Foo {
  class Bar
  def foo()(using QuoteContext) = {
    Type[Bar] // error
  }
}
