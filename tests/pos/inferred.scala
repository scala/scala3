object Inferred {

  def foo[T](x: T): T = x

  val x = foo(1)

  val y = foo("abc")

  def bar[U](xs: List[U]): List[U] = xs

  val n = Nil

  val nn = bar(Nil)

  val ints: List[Int] = 1 :: Nil

  val a = if (1 == 0) Nil else ints

}
