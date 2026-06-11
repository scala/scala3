inline trait A[T](x: T):
  def foo: T = x

class B extends A[Int](15):
    val y = 1

def h(x: B) = x.foo
