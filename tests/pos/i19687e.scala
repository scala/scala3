// Nested case class with uninhabitable field
enum Eq[A, B]:
  case Refl[A]() extends Eq[A, A]

case class Inner[A](w: Eq[A, String])
case class Outer[A](inner: Inner[A])

sealed trait T[A]
case class X[A](o: Outer[A]) extends T[A]
case class Y[A](w: Eq[A, Int]) extends T[A]

def test(t: T[Int]): Int =
  t match
    case Y(Eq.Refl()) => 0
