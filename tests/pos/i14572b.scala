import scala.compiletime.ops.int.*

type MinusOneModuloThree[I <: Int] <: Int = I match
  case S[n] => n % 3
  case 0    => 2


val _: MinusOneModuloThree[5] = 1         // (5 - 1) mod 3  = 1
val _: MinusOneModuloThree[0] = 2         // (0 - 1) mod 3  = 2