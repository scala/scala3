enum List[+T] {
  case Cons(x: T, xs: List[T])
  case Snoc[U](xs: List[U], x: U) // error: case with type parameters needs extends clause
}

enum class X {
  case Y // error: case not allowed here
}

enum E1[T] {
  case C // error: cannot determine type argument
}

enum E2[+T, +U >: T] {
  case C // error: cannot determine type argument
}

enum E3[-T <: Ordered[T]] {
  case C // error: cannot determine type argument
}
