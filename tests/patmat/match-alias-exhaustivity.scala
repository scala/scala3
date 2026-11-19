// Exhaustivity must prune a child whose type argument is declared through a
// parameterised alias applying a match type to the alias's own parameters.
sealed trait Release
object Release:
  case object R1 extends Release
  case object R2 extends Release
  case object R3 extends Release
  case object R4 extends Release

  type From1 = (R1.type | R2.type | R3.type | R4.type) & Release
  type From3 = (R3.type | R4.type) & Release
  type Upto2 = (R1.type | R2.type) & Release
  type Upto4 = (R1.type | R2.type | R3.type | R4.type) & Release

  type Since[X <: Release] = X match
    case R1.type => From1
    case R3.type => From3

  type Until[X <: Release] = X match
    case R2.type => Upto2
    case R4.type => Upto4

  infix type to[A <: Release, B <: Release] = Since[A] & Until[B]

import Release.*

sealed trait Feature[-R <: Release]
object Feature:
  final case class Old(n: Int) extends Feature[R1.type to R2.type]
  final case class New(n: Int) extends Feature[R3.type to R4.type]

// Old covers R1..R2, so it cannot occur at R4.
def read(f: Feature[R4.type]): Int = f match
  case Feature.New(n) => n
