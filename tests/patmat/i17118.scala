class Foo

sealed trait T[U]
final case class G[A <: Foo](i: A) extends T[A]

class Test:
  def f[X](t1: T[X]): X = t1 match
    case G(i) => i
