// As match-alias-exhaustivity, but the range is computed by recursive match types behind the alias.
sealed trait Release
object Release:
  case object R1 extends Release
  case object R2 extends Release
  case object R3 extends Release
  case object R4 extends Release

  type All = (R1.type, R2.type, R3.type, R4.type)

  type Taken[X <: Release, T <: Tuple] <: Tuple = T match
    case X *: _ => X *: EmptyTuple
    case h *: r => h *: Taken[X, r]

  type Dropped[X <: Release, T <: Tuple] <: Tuple = T match
    case X *: r => X *: r
    case _ *: r => Dropped[X, r]

  infix type to[A <: Release, B <: Release] =
    Tuple.Union[Dropped[A, Taken[B, All]]] & Release

import Release.*

sealed trait Feature[-R <: Release]
object Feature:
  final case class Old(n: Int) extends Feature[R1.type to R2.type]
  final case class New(n: Int) extends Feature[R3.type to R4.type]

def read(f: Feature[R4.type]): Int = f match
  case Feature.New(n) => n
