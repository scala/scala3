import quoted._

def foo() given QuoteContext = {
  type C
  '[C] // error
}
