import quoted._

def foo() with QuoteContext = {
  class C
  '[C] // error
}
