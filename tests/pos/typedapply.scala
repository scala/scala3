object typedapply {

  def foo[X, Y](x: X, y: Y) = (x, y)

  foo(1, "abc")

  foo[Int, String](1, "abc")

  val x = foo[Int, String] _

}
