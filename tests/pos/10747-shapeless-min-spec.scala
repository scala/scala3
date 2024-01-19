trait Monoidal {
  type to[_] <: Tuple
}

object eithers extends Monoidal {
  class Wrap[T]

  type to[t] <: Tuple = Wrap[t] match {
    case Wrap[Nothing] => EmptyTuple
    case Wrap[other] => other match
      case Either[hd, tl] => hd *: to[tl]
  }
}
