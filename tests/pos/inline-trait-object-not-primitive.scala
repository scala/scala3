object Hop

inline trait A[T](x: T):
  def foo: T = x

class B extends A[Hop.type](Hop):
    val y = 1

def h(x: B) = x.foo
