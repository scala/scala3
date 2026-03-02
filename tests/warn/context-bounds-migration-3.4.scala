//> using options -source 3.4

class C[T]
def foo[X: C] = ()

given [T]: C[T] = C[T]()

def Test =
  foo(C[Int]())  // warn
  foo(using C[Int]()) // ok
