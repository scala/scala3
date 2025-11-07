trait A[+T]:
  def x: T
final class B[+T] extends A[T]:
  def x: T = ???
  def y: T = ???

def f[T](a: A[T]): T = a match {
  case b: B[T] => b.y  // compiles without warning
  case _ => a.x
}
