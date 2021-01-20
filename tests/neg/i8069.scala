trait A with
  type B

enum Test with
  case Test(a: A, b: a.B)   // error: Implementation restriction: case classes cannot have dependencies between parameters

case class Test2(a: A, b: a.B) // error: Implementation restriction: case classes cannot have dependencies between parameters

