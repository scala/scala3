// Two type params with cross-constrained fields
enum Eq[A, B]:
  case Refl[A]() extends Eq[A, A]

case class Linked[A, B](fst: Eq[A, Int], snd: Eq[B, String])

sealed trait T[A, B]
case class W[A, B](link: Linked[A, B]) extends T[A, B]
case class V[A, B](wa: Eq[A, Int], wb: Eq[B, Int]) extends T[A, B]

def test(t: T[Int, Int]): Int =
  t match
    case V(Eq.Refl(), Eq.Refl()) => 0
