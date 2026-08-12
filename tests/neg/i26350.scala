// https://github.com/scala/scala3/issues/26350
// Failing `compiletime.ops` folds must still be reported when a branch is
// genuinely selected, on a direct use, or when declared bounds do not align.
import scala.compiletime.ops.int.*

type AnInt[T] <: Int = T match
  case Int => T & Int
  case _   => 1 / 0

val a: 1 / 0 = 5           // error
val b: AnInt[String] = ??? // error

type Bad[T] <: String = T match
  case Int => "i"
  case _   => 1 / 0        // error
