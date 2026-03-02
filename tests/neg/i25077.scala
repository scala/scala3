//> using options "-Wconf:msg=multiple val:e"

def f(x: Int): (Int, Int) = (1, x)

val result: Iterable[(Int, Int)] =
  for // error behavior change in 3.8
    (k, v) <- Map(1 -> 1, 2 -> 1, 3 -> 1)
    x      = k + v
    (a, b) = f(x)
    (y, z) <- Map(42 -> 27)
  yield (a+y, b+z)
