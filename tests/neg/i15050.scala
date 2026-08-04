import OpaqueBug.*
def g(n: Counter): Counter = n
object OpaqueBug:
  opaque type Counter = Int
  val initial: Counter = 42
  // LTS only, probably fixed by https://github.com/scala/scala3/pull/19730
  def f(n: Int): Int = g(n) + initial // error 
  @main def run = println(f(21))
