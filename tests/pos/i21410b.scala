object Test:
  def foo[T](x: Option[T]): T = ???
  def foo[T <: Tuple](x: T): Tuple.Map[T, List] = ???

  val tup: (Int, String) = (1, "")

  val x = foo(tup)
  val y: (List[Int], List[String]) = x

  val x2: (List[Int], List[String]) = foo(tup) // error
