enum AList[+A] {
  case Cons(head: A, tail: AList[A])
  case Nil
}

object AList {
  extension [A](l: AList[A]) def sum(using numeric: Numeric[A]): A = l match {
    case Cons(x, xs) => numeric.plus(x, xs.sum(using numeric))
    case Nil         => numeric.zero
  }
}