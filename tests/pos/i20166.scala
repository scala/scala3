import scala.compiletime.ops.int.*

// NOTE ops.int.S is documented as equivalent to MyS

type MyS[X] = X match
  case 0 => 1
  case 1 => 2
  case 2 => 3

type M[I <: Int] = 4 match
  case 1 - 1 => "0"
  case MyS[I] => "2"
  case S[I] => "2" // Not provablyDisjoint before changes
  case 2 + I => "3"
  case I + 3 => "4"

val _: M[1] = "4"


type M2[I <: Int, P] = I match
  case P => "b"
  case _ => "c"

val _: M2[5, 2 + 3] = "b"
