//> using options -Werror

sealed trait A[T]
class B[T] extends A[T]

class Test {
  def f(x: B[Int]) = x match { case _: A[Int] if true => }

  def g(x: A[Int]) = x match { case _: B[Int] => }
}
