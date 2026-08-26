
enum E[+T] extends java.lang.Enum[E[_]] { // error: An enum extending java.lang.Enum cannot have type parameters
  case S1, S2
  case C() extends E[Int]
}

enum E2 extends java.lang.Enum[E2] {
  case S1, S2
  case C() extends E2 // error: parameterized case is not allowed
}
