def widen[T](x: T): T = x

def f[T <: Tuple](t: T): Unit =
  val tl: Tuple.Map[T, List] = widen(t).map([X] => (x: X) => List(x))
