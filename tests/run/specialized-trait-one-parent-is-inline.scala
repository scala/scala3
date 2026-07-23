//> using options -language:experimental.specializedTraits

inline trait Foo[T: Specialized]
inline trait A[T](x: T):
  def foo: T = x

class B extends A[Int](15), Foo:
    val y = 1

def h(x: B) = x.foo

@main def Test =
    val b = B()
    assert(h(b) == 15)
    assert(b.y == 1)
