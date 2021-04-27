trait Sealed[A]

enum Foo derives Sealed { // error
  case A, B, C
}