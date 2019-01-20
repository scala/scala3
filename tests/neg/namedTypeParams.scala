class C[T]
class D[type T] // error: identifier expected, but `type` found

object Test {

  val x: C[T = Int] = // error:  ']' expected, but `=` found // error
    new C[T = Int] // error:  ']' expected, but `=` found // error

  class E extends C[T = Int] // error: ']' expected, but `=` found // error
  class F extends C[T = Int]() // error: ']' expected, but `=` found // error

  def f[X, Y](x: X, y: Y): Int = ???

  f[X = Int, String](1, "") // error // error
  f[X = Int][X = Int][Y = String](1, "") // error: illegal repeated type application

  f[X = Int][Y = String](1, "") // error: illegal repeated type application
  f[X = Int][String](1, "") // error: illegal repeated type application

  f[Y = String][X = Int](1, "") // error: illegal repeated type application
  f[Y = String][Int](1, "") // error: illegal repeated type application
}
