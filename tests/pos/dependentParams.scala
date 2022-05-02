class LT[-X, +Y]
object LT:
 given lt[X <: Y, Y]: LT[X, Y] = ???

class Contra[-A]
object Test:
  def foo[S <: T, T](using LT[T, S]): Unit = {}
  foo
