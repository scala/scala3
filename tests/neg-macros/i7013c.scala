import quoted._

def foo()(using QuoteContext) = {
  type C
  Type[C] // error
}
