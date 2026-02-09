object A:
  class Box(value: => Int)

  def f(a: => Int): Box =
    val b = a
    Box(b)

  val box = f(n)   // warn
  val n = 10

object B:
  class Box(value: Int)

  def f(a: => Int): Box =
    lazy val b = a
    Box(b)

  val box = f(n)
  val n = 10
