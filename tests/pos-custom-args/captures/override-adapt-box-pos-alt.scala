import language.experimental.captureChecking

class IO

abstract class A[X] {
  def foo(x: Unit): X
  def bar(op: X => Int): Int
}

class C

def test(io: {*} IO) = {
  class B extends A[{io} C] {  // X =:= {io} C
    def foo(x: Unit): {io} C = ???
    def bar(op: ({io} C) => Int): Int = 0
  }
}
