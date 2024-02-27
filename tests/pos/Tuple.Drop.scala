import compiletime.ops.int.*

type Drop[T <: Tuple, N <: Int] <: Tuple = N match
  case 0 => T
  case S[n1] => T match
    case EmptyTuple => EmptyTuple
    case x *: xs => Drop[xs, n1]
