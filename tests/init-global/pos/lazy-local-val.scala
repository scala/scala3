object Test:
  class Box(value: => Int)

  def f(a: => Int): Box =
    lazy val b = a
    Box(b)

  val box = f(n)
  val n = 10
