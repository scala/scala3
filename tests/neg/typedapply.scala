object typedapply {

  def foo[X, Y](x: X, y: Y) = (x, y)

  foo[Int](1, "abc")  // error: wrong number of type parameters

  foo[Int, String, String](1, "abc") // error: wrong number of type parameters

  def bar(x: Int) = x

  bar[Int](1) // error: does not take parameters

}
