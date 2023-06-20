import scala.compiletime.ops.int, int.-

type Count[N, T] <: Tuple = (N, T) match
  case (0, T) => EmptyTuple
  case (N, T) => T *: Count[N - 1, T]

val a: Count[3, Int] = (1, 2, 3)
