import quoted._

def foo() with QuoteContext = {
  type C
  '[C] // error
}
