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

  transparent def add(x: Nat, y: Nat): Nat = x match {
    case Z => y
    case S(x1) => S(add(x1, y))
  }

  val x = S(S(Z))
  val y = add(x, x)
  val z: S[S[S[S[Z]]]] = y

  transparent def concat[T, N1 <: Nat, N2 <: Nat](xs: Vec[T, N1], ys: Vec[T, N2]): Vec[T, _] = {
    erased val length = Typed(add(erasedValue[N1], erasedValue[N2]))
    Vec[T, length.Type](xs.elems ++ ys.elems)
  }

  val xs = Vec[Int, S[S[Z]]](List(1, 2))
  val ys = Vec[Int, S[Z]](List(3))
  val zs = concat(xs, ys)
  val zsc: Vec[Int, S[S[S[Z]]]] = zs
}

