object Test {

  type LF = (=> Int) => Int

  def f(x: => Int) = x * x

  val x: LF = f

  def g = 3

  f(11)
  x(g)
  x(11)

}
