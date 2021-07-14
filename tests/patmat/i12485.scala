case class A(a: A)

def foo(x: A) = x match
  case A(a) =>