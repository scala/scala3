import language.experimental.captureChecking

abstract class A[X] {
  def foo(x: X): X
}

class IO
class C

def test(io: {*} IO) = {
  class B extends A[{io} C] {  // X =:= {io} C  // error
    override def foo(x: {io} C): {io} C = ???  // error
  }
}
