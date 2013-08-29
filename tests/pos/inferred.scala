object Inferred {

  def foo[T](x: T): T = x

  val x = foo(1)

  val y = foo("abc")

}
