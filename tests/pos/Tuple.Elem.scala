import compiletime.ops.int.*

type Elem[T <: Tuple, I <: Int] = T match
  case h *: tail =>
    I match
      case 0 => h
      case S[j] => Elem[tail, j]
