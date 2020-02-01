import quoted._

def foo()(using QuoteContext) = {
  type C
  '[C] // error
}
