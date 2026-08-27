//> using options -language:experimental.specializedTraits
import scala.quoted.*

inline def foo(): Unit =
  ${fooImpl}

def fooImpl(using Quotes): Expr[Unit] =
  '{
    val x = new A[Int]() {}  
    assert(x.foo(15) == 10)
    assert(x.bar == "A$impl$scala$Int")
  }

inline trait A[T: Specialized]:
    def foo(x: T) = 10
    def bar = Thread.currentThread.getStackTrace()(1).getClassName()
