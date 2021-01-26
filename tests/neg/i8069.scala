trait A:
  type B

enum Test:
  case Test(a: A, b: a.B)   // error: Implementation restriction: case classes cannot have dependencies between parameters

case class Test2(a: A, b: a.B) // error: Implementation restriction: case classes cannot have dependencies between parameters

