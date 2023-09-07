import language.experimental.captureChecking

class IO

abstract class A[X, Y] {
  def foo(x: Unit): X
  def bar(x: Int, y: IO^{}): X
  def baz(x: Y): X
}

class C

def test(io: IO^) = {
  class B extends A[C^{io}, C^{}] {  // X =:= {io} C
    override def foo(x: Unit): C^{io} = ???
    override def bar(x: Int, y: IO^{}): C^{io} = ???
    override def baz(x: C^{}): C^{io} = ???
  }
}
