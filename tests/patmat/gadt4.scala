sealed trait Nat[+T]
case class Zero() extends Nat[Nothing]
case class Succ[T]() extends Nat[T]

// +N is incorrect, as in `foo` we can have `N = Zero | Succ[Zero]`,
// then it's correct for exhaustivity check to produce two warnings.
sealed trait Vect[+N <: Nat[?], +T]
case class VN[T]() extends Vect[Zero, T]
case class VC[T, N <: Nat[?]](x: T, xs: Vect[N, T]) extends Vect[Succ[N], T]

object Test {
  def foo[N <: Nat[?], A, B](v1: Vect[N, A], v2: Vect[N, B]) = (v1, v2) match {
    case (VN(), VN()) => 1
    case (VC(x, xs), VC(y, ys)) => 2
  }
}
