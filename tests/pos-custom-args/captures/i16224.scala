import language.experimental.captureChecking
class Delta:
  val value = 1

  def f(v: Int)(using delta: Delta): Int =
    v + delta.value

  def run(): Unit =
    val delta = Delta()
    val x: Map[Char, Int] = Map(
      'a' -> 0,
      'b' -> 1
    )
    val y: Map[Char, Int] = x.map((k, v) => (k, f(v)(using delta)))