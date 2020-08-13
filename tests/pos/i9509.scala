enum AList[+A] {
  case Cons[X](head: X, tail: AList[X]) extends AList[X]
  case Nil
}

object AList {
  extension [A](l: AList[A]) def sum(using numeric: Numeric[A]): A = l match {
    case Cons(x, xs) => numeric.plus(x, xs.sum(using numeric))
    case Nil         => numeric.zero
  }
}
