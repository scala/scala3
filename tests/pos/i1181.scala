object Test {
  def foo[M[_]](x: M[Int]) = x

  type Alias[A] = (A, A)
  val x: Alias[Int] = (1, 2)

  foo[Alias](x) // ok
  foo(x) // ok in scalac but fails in dotty with:
         // error: type mismatch:
         //  found   : (Int, Int)
         //  required: M[Int]
}
