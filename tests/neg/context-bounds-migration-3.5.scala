//> using options -source 3.5

class C[T]
def foo[X: C] = ()

given [T]: C[T] = C[T]()

def Test =
  foo(C[Int]()) // error
  foo(using C[Int]()) // ok
