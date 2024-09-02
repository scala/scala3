import OpaqueBug.*
def g(n: Counter): Counter = n
object OpaqueBug:
  opaque type Counter = Int
  val initial: Counter = 42
  def f(n: Int): Int = g(n) + initial
  @main def run = println(f(21))
