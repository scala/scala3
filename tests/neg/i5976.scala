object Test {
  def f(i: => Int) = i + i
  val res = List(42).map(f)

  val g: (=> Int) => Int = f
  val h: Int => Int = g // error
}
