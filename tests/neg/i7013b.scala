import quoted._

class Foo {
  class Bar
  def foo() with QuoteContext = {
    '[Bar] // error
  }
}
