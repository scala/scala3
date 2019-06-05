
enum E[+T] extends compat.JEnum[E[_]] {
  case S1, S2
  case C() extends E[Int]  // error: parameterized case is not allowed
}
