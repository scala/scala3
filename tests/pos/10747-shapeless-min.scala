trait Monoidal {
  type to[_] <: Tuple
}

object eithers extends Monoidal {
  class Wrap[T]

  type to[t] <: Tuple = Wrap[t] match {
    case Wrap[Either[hd, tl]] => hd *: to[tl]
    case Wrap[Nothing] => EmptyTuple
  }
}
