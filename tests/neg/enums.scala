enum List[+T] {
  case Cons(x: T, xs: List[T])
  case Nil // error: illegal enum value
  case Snoc[U](xs: List[U], x: U) // error: case with type parameters needs extends clause
}

enum class X {
  case Y // error: case not allowed here
}
