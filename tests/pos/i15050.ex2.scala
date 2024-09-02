opaque type Counter = Int
def g(n: Counter): Counter = n
object OpaqueBug:
  val initial: Counter = 42 // was error: Found: (42 : Int)
  def f(n: Int): Int = g(n) + initial // was error: Found: (n : Int) Required: Counter
  @main def run = println(f(21))
