object Test {

  def f[X]: (Set[X], Set[X]) = ???

  val a = if (true) f else (Set[Int](), Set[String]())
}
