// Both fields inhabitable, one case missing
enum Eq[A, B]:
  case Refl[A]() extends Eq[A, A]

sealed trait W[A]
case class A1[A](witness: Eq[A, Int]) extends W[A]
case class B1[A](witness: Eq[A, Int]) extends W[A]

def test(w: W[Int]): Int =
  w match // warn
    case A1(Eq.Refl()) => 0
