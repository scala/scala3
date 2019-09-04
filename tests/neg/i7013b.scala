import quoted._

class Foo {
  class Bar
  def foo() given QuoteContext = {
    '[Bar] // error
  }
}
