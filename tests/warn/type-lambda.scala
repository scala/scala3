

trait A[T]
trait B[T] extends A[T]

object Test {
  def foo(x: ([X] =>> A[X])[Any]) = x match {
    case x: ([X] =>> B[X])[Any] =>
    case _ =>
  }

  def bar(x: ([X] =>> A[X])[Any]) = x match {
    case x: ([X] =>> B[Nothing])[Any] => // warn
    case _ =>
  }
}
