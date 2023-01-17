import language.experimental.captureChecking

class IO
class C

object Test1 {
  abstract class A[X] { this: {} A[X] =>
    def foo(x: X): X
  }

  def test(io: {*} IO) = {
    class B extends A[{io} C] {  // X =:= {io} C // error
      override def foo(x: {io} C): {io} C = ???
    }
  }
}

def Test2(io: {*} IO, fs: {io} IO, ct: {*} IO) = {
  abstract class A[X] { this: {io} A[X] =>
    def foo(x: X): X
  }

  class B1 extends A[{io} C] {
    override def foo(x: {io} C): {io} C = ???
  }

  class B2 extends A[{ct} C] {  // error
    override def foo(x: {ct} C): {ct} C = ???
  }

  class B3 extends A[{fs} C] {
    override def foo(x: {fs} C): {fs} C = ???
  }
}

def Test3(io: {*} IO, ct: {*} IO) = {
  abstract class A[X] { this: {*} A[X] =>
    def foo(x: X): X
  }

  class B1 extends A[{io} C] {
    override def foo(x: {io} C): {io} C = ???
  }

  class B2 extends A[{io, ct} C] {
    override def foo(x: {io, ct} C): {io, ct} C = ???
  }
}
