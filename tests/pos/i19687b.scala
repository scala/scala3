// Multiple fields where one is uninhabitable
enum Eq[A, B]:
  case Refl[A]() extends Eq[A, A]

case class Multi[A](x: Int, witness: Eq[A, String], y: String)

sealed trait T[A]
case class P[A](m: Multi[A]) extends T[A]
case class Q[A](witness: Eq[A, Int]) extends T[A]

def test(t: T[Int]): Int =
  t match
    case Q(Eq.Refl()) => 0
