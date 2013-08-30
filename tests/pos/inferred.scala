object Inferred {

  def foo[T](x: T): T = x

  val x = foo(1)

  val y = foo("abc")

  def bar[U](xs: List[U]): List[U] = xs

  val n = Nil

  val nn = bar(Nil)

}
