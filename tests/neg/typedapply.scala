object typedapply {

  def foo[X, Y](x: X, y: Y) = (x, y)

  foo[Int](1, "abc")  // error: wrong number of type parameters

  foo[Int, String, String](1, "abc") // error: wrong number of type parameters

  def fuz(x: Int, y: String) = (x, y)

  val x1 = fuz.curried // OK
  val x2: Int => String => (Int, String) = x1

  def bar(x: Int) = x

  bar[Int](1) // error: does not take type parameters

  bar.baz // error: baz is not a member of Int => Int


}


