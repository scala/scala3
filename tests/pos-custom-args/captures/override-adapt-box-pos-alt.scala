import language.experimental.captureChecking

class IO

abstract class A[X] {
  def foo(x: Unit): X
  def bar(op: X => Int): Int
}

class C

def test(io: IO^) = {
  class B extends A[C^{io}] {  // X =:= {io} C
    def foo(x: Unit): C^{io} = ???
    def bar(op: (C^{io}) => Int): Int = 0
  }
}
