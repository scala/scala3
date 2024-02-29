import language.experimental.captureChecking

abstract class A[X] { this: A[X]^{} =>
  def foo(x: X): X
}

class IO
class C

def test(io: IO^) = {
  class B extends A[C^{io}] {  // X =:= {io} C  // error
    override def foo(x: C^{io}): C^{io} = ???
  }
}
