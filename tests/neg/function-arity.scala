object Test {

  // From #873:

  trait X extends Function1[Int, String]
  implicit def f2x(f: Function1[Int, String]): X = ???
  ({case _ if "".isEmpty => 0} : X) // error: expected String, found Int

  // Tests where parameter list cannot be made into a pattern

  def unary[T](x: T => Unit) = ???
  unary((x, y) => ())   // error

  unary[(Int, Int)]((x, y) => ())

  unary[(Int, Int)](() => ())   // error
  unary[(Int, Int)]((x, y, _) => ()) // error

  unary[(Int, Int)]((x: String, y) => ()) // error


}
