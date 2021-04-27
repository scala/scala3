class Foo[?] // error
class Bar[M[?]] // error
def foo[?] = {} // error
def bar[M[?]] = {} // error

type A[?] = Int // error
type B = [?] =>> Int // error
type C = [?] => Int => Int // error
type D = [X[?]] =>> Int // error
type E = [X[?]] => Int => Int // error

enum F {
  case ?() // error
}
object G {
  class ? { val x = 1 } // error
}
object H {
  trait ? // error
}
object I {
  type ? = Int // error
}
object J {
  enum ? { // error
    case X
  }
}
object K {
  class Foo[T]
  given ?[T]: Foo[T]() // error
}
