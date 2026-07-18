
class A
object A:
  given A = ???

class B[X]
object B:
  given g[T]: B[T] = ???

object Test:
  def foo[X >: A] = summon[X] // was error
  def bar[F[T] >: B[T]] = summon[F[Int]] // was error
