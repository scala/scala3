enum Nat {
  case Zero
  case Succ[N <: Nat](n: N)
}
import Nat._

inline def toInt(inline n: Nat): Int = inline n match {
  case Zero => 0
  case Succ(n1) => toInt(n1) + 1
}

val natTwoA = toInt(Succ[Succ[Zero.type]](Succ(Zero)))
val natTwoB = toInt(Succ(Succ(Zero)): Succ[Succ[Zero.type]])
