import language.experimental.captureChecking

class IO

abstract class A[X, Y] {
  def foo(x: Unit): X
  def bar(x: Int, y: {} IO): X
  def baz(x: Y): X
}

class C

def test(io: {*} IO) = {
  class B extends A[{io} C, {} C] {  // X =:= {io} C
    override def foo(x: Unit): {io} C = ???
    override def bar(x: Int, y: {} IO): {io} C = ???
    override def baz(x: {} C): {io} C = ???
  }
}
