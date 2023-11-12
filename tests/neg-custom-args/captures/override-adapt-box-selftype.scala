import language.experimental.captureChecking

class IO
class C

object Test1 {
  abstract class A[X] { this: A[X] =>
    def foo(x: X): X
  }

  def test(io: IO^) = {
    class B extends A[C^{io}] {  // X =:= {io} C // error
      override def foo(x: C^{io}): C^{io} = ???
    }
  }
}

def Test2(io: IO^, fs: IO^{io}, ct: IO^) = {
  abstract class A[X] { this: A[X]^{io} =>
    def foo(x: X): X
  }

  class B1 extends A[C^{io}] {
    override def foo(x: C^{io}): C^{io} = ???
  }

  class B2 extends A[C^{ct}] {  // error
    override def foo(x: C^{ct}): C^{ct} = ???
  }

  class B3 extends A[C^{fs}] {
    override def foo(x: C^{fs}): C^{fs} = ???
  }
}

def Test3(io: IO^, ct: IO^) = {
  abstract class A[X] { this: A[X]^ =>
    def foo(x: X): X
  }

  class B1 extends A[C^{io}] {
    override def foo(x: C^{io}): C^{io} = ???
  }

  class B2 extends A[C^{io, ct}] {
    override def foo(x: C^{io, ct}): C^{io, ct} = ???
  }
}
