import quoted._

def foo()(using Quotes) = {
  type C
  Type[C] // error
}
