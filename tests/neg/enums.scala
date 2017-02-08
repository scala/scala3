enum List[+T] {
  case Cons(x: T, xs: List[T])
  case Nil // error: illegal enum value
}

enum class X {
  case Y // error: case not allowed here
}
