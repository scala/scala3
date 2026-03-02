trait A[+T]:
  def x: T
trait B[+T] extends A[T]:
  def y: T

object Troll extends A[Int] with B[Any]:
  def x: Int = 0
  def y: Any = ""

def f[T](a: A[T]): T = a match {
  case b: B[T] => b.y  // warn
  case _ => a.x
}
