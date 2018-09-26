object typelevel {
  case class Typed[T](value: T) { type Type = T }
  erased def erasedValue[T]: T = ???
}

trait Nat
case object Z extends Nat
case class S[N <: Nat](n: N) extends Nat

case class Vec[T, N <: Nat](elems: List[T])

object Test {
  import typelevel._
  type Z = Z.type

  inline def add(x: Nat, y: Nat): Nat = inline x match {
    case Z => y
    case S(x1) => S(add(x1, y))
  }

  val x = S(S(Z))
  val y = add(x, x)
  val z: S[S[S[S[Z]]]] = y

  inline def concat[T, N1 <: Nat, N2 <: Nat](xs: Vec[T, N1], ys: Vec[T, N2]): Vec[T, _] = {
    val length = Typed(add(erasedValue[N1], erasedValue[N2]))
    Vec[T, length.Type](xs.elems ++ ys.elems)
  }
}

