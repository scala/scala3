//> using options -language:experimental.erasedDefinitions

case class Ev(i: Int)

def foo(x: Int)(erased ev: Ev): Int = x + 1

object A:
  erased var ev = Ev(0)
  ev = Ev(1)
  foo(1)(ev)

