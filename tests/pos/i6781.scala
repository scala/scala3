enum Nat {
  case Zero
  case Succ[N <: Nat](n: N)
}
import Nat._

inline def toInt(n: => Nat): Int = inline n match {
  case Zero => 0
  case Succ(n1) => toInt(n1) + 1
}

val natTwo = toInt(Succ(Succ(Zero)))
