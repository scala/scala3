import quoted._

def foo(using s: Scope): Any = {
  type C
  '[C] // error
}
