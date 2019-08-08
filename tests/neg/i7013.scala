import quoted._

def foo() given QuoteContext = {
  class C
  '[C] // error
}
