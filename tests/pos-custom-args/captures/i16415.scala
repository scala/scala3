abstract class A[X]:
  def foo(x: X): X

class IO
class C
def test(io: IO^) =
  class B extends A[C^{io}]:    // error, but should work
    override def foo(x: C^{io}): C^{io} = ???
