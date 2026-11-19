// As match-alias-exhaustivity, but one child's range is an application of a match
// alias that does not reduce, because its scrutinee is the child's own type
// parameter. Exhaustivity must then widen it to the alias's declared bound, which
// keeps the child rather than pruning it unsoundly.
sealed trait Release
object Release:
  case object R1 extends Release
  case object R2 extends Release
  case object R3 extends Release
  case object R4 extends Release

  type Since[X <: Release] <: Release = X match
    case R1.type => R1.type | R2.type | R3.type | R4.type
    case R3.type => R3.type | R4.type

import Release.*

sealed trait Feature[-R <: Release]
object Feature:
  // `Since[X]` is stuck, so `Old` is approximated by its bound, `Release`.
  final case class Old[X <: Release](n: Int) extends Feature[Since[X]]
  final case class New(n: Int) extends Feature[Since[R3.type]]

def read(f: Feature[R4.type]): Int = f match
  case Feature.New(n) => n
