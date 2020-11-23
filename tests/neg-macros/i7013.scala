import quoted._

def foo()(using Quotes) = {
  class C
  Type[C] // error
}
