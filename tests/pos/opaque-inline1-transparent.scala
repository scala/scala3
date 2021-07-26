
object refined:
  opaque type Positive = Int
  transparent inline def Positive(value: Int): Positive = f(value)
  def f(x: Positive): Positive = x

object test:
  def run: Unit =
    val x = 9
    val nine = refined.Positive(x)

