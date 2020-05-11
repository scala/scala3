import quoted._

def foo(using s: Scope)(): Any = {
  class C
  '[C] // error
}
