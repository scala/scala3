sealed trait Nat
case class S(n: Nat) extends Nat
case object Z extends Nat

inline def pred(n: Nat) = inline n match {
  case S(m) => m
  case Z =>
    compiletime.error("n cannot be Z")
}

class Test {
  pred(Z) // error
}
