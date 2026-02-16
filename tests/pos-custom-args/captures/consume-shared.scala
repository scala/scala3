import caps.*
class C extends SharedCapability

def foo(consume x: C): C = x

def test(consume x: C) =
  val y: C = C()
  foo(x)
  foo(y)
