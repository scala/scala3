object typelevel {
  case class Typed[T](value: T) { type Type = T }
  erased def erasedValue[T]: T = ???
}

sealed trait Nat
case object Z extends Nat
case class S[N <: Nat](n: N) extends Nat

object Nat {
  type Z = Z.type
  transparent def fromInt(n: Int): Nat = n match {
    case 0 => Z
    case n1 => S(fromInt(n - 1))
  }
}

import Nat.Z

sealed trait Vec[+A] { type Len <: Nat }
case object VNil extends Vec[Nothing] { type Len = Z }
case class VCons[+A, TL <: Vec[A]](hd: A, tl: TL) extends Vec[A] { type Len = S[tl.Len]}

object Vec {
  type VNil = VNil.type
}

import Vec.VNil

object Concat {
  transparent def concat[A](xs: Vec[A], ys: Vec[A]): Vec[A] =
    xs match {
      case VNil => ys
      case VCons(hd, tl) => VCons(hd, concat(tl, ys))
    }
}

import Concat.concat

object Test {
  val v1 = VCons(1, VCons(2, VNil))
  val v2 = VCons(3, VCons(4, VCons(5, VNil)))
  val v3 = concat(v1, v2)
  val v3l: v3.Len = Nat.fromInt(5)
}