// Recursive case class, non-exhaustive (should not cause infinite loop)
case class Rec(r: Rec, x: Int)

sealed trait W
case class A1(rec: Rec) extends W
case class B1(x: Int) extends W

def test(w: W): Int =
  w match // warn
    case B1(_) => 0
