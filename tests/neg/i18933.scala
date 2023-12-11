enum Extends[A, B]:
  case A(a: A)
  infix case B(b: B) // error // error
