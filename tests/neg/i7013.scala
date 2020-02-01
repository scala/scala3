import quoted._

def foo()(using QuoteContext) = {
  class C
  '[C] // error
}
