// Some cases eliminated by uninhabitable fields, but inhabitable case still missing
enum Eq[A, B]:
  case Refl[A]() extends Eq[A, A]

sealed trait W[A]
case class A1[A](w: Eq[A, String]) extends W[A]
case class B1[A](w: Eq[A, Int]) extends W[A]
case class C1[A](w: Eq[A, Int]) extends W[A]

def test(w: W[Int]): Int =
  w match // warn
    case B1(Eq.Refl()) => 0
