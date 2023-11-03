object Test:
  class Box(val x: Int)

  def recur(a: => Box, b: => Box): Int =
    a.x + recur(a, b) + b.x

  recur(Box(1), Box(2))