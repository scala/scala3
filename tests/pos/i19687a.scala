// Sealed trait version of i19687
sealed trait Eq2[A, B]
case class Refl2[A]() extends Eq2[A, A]

sealed trait T2[A]
case class S2[A](witness: Eq2[A, String]) extends T2[A]
case class I2[A](witness: Eq2[A, Int]) extends T2[A]

def test(tagged: T2[Int]): Int =
  tagged match
    case I2(Refl2()) => 0
