import quoted._

def foo()(using QuoteContext) = {
  class C
  Type[C] // error
}
