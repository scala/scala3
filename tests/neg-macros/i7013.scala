import quoted.*

def foo()(using Quotes) = {
  class C
  Type[C] // error
}
