import quoted.*

def foo()(using Quotes) = {
  type C
  Type[C] // error
}
