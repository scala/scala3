class C[T]
class D[type T] // error: identifier expected, but `type` found

object Test {

  val x: C[T = Int] = // error:  ']' expected, but `=` found // error
    new C[T = Int] // error:  ']' expected, but `=` found // error

  class E extends C[T = Int] // error: ']' expected, but `=` found // error
  class F extends C[T = Int]() // error: ']' expected, but `=` found // error

}
