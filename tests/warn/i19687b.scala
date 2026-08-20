// Multiple fields all inhabitable, case still missing
enum Eq[A, B]:
  case Refl[A]() extends Eq[A, A]

case class Multi[A](x: Int, witness: Eq[A, Int], y: String)

sealed trait W[A]
case class P1[A](m: Multi[A]) extends W[A]
case class Q1[A](w: Eq[A, Int]) extends W[A]

def test(w: W[Int]): Int =
  w match // warn
    case Q1(Eq.Refl()) => 0
