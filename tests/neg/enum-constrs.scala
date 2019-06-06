
enum E[+T] extends java.lang.Enum[E[_]] {
  case S1, S2
  case C() extends E[Int]  // error: parameterized case is not allowed
}
