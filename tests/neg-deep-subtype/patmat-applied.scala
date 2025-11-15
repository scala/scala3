//> using options -Werror

class A[-T]
class B[T] extends A[T]

object Test {
  def foo(x: A[Null]) = x match {
    case x: B[Null] =>
    case _ =>
  }
}

def bar[T](x: List[T]) = x.isInstanceof[List[Int]] // error
