object typedapply {

  def foo[X, Y](x: X, y: Y) = (x, y)

  foo[Int](1, "abc")                          // error

  foo[Int, String, String](1, "abc")          // error

  def bar(x: Int) = x

  bar[Int](1)                                 // error

  def baz[X >: Y, Y <: String](x: X, y: Y) = (x, y)

  baz[Int, String](1, "abc")                  // error

}
