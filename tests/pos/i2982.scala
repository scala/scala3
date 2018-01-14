
object A {
  def fun[E](a: A[E]): Unit = ()
  fun(new A[Int])
}
class A[-X]