import Nat._
import compiletime._

enum Nat:
  case Zero
  case Succ[N <: Nat](n: N)

inline def toIntTypeLevel[N <: Nat]: Int = inline erasedValue[N] match
  case _: Zero.type => 0
  case _: Succ[n]   => toIntTypeLevel[n] + 1

inline def toInt(inline nat: Nat): Int = inline nat match
  case nat: Zero.type => 0
  case nat: Succ[n]   => toInt(nat.n) + 1

inline def toIntUnapply(inline nat: Nat): Int = inline nat match
  case Zero    => 0
  case Succ(n) => toIntUnapply(n) + 1

inline def toIntTypeTailRec[N <: Nat, Acc <: Int]: Int = inline erasedValue[N] match
  case _: Zero.type => constValue[Acc]
  case _: Succ[n]   => toIntTypeTailRec[n, S[Acc]]

inline def toIntErased[N <: Nat](inline nat: N): Int = toIntTypeTailRec[N, 0]

@main def Test: Unit =
  println("erased value:")
  assert(toIntTypeLevel[Succ[Succ[Succ[Zero.type]]]] == 3)
  println("type test:")
  assert(toInt(Succ(Succ(Succ(Zero)))) == 3)
  println("unapply:")
  assert(toIntUnapply(Succ(Succ(Succ(Zero)))) == 3)
  println("infer erased:")
  assert(toIntErased(Succ(Succ(Succ(Zero)))) == 3)
