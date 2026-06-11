object MyObject

inline trait A[T](x: T):
  def foo: T = x

class B extends A[MyObject.type](MyObject):
    val y = 1

def h(x: B) = x.foo
