// All children eliminated except the matched one
enum Eq[A, B]:
  case Refl[A]() extends Eq[A, A]

sealed trait T[A]
case class A1[A](w: Eq[A, String]) extends T[A]
case class B1[A](w: Eq[A, Boolean]) extends T[A]
case class C1[A](w: Eq[A, Int]) extends T[A]

def test(t: T[Int]): Int =
  t match
    case C1(Eq.Refl()) => 0
