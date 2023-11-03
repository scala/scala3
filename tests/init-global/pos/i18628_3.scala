import scala.annotation.init.widen

object Test:
  class Box(val x: Int)

  def recur(a: => Box, b: => Box): Int =
    a.x + recur(a: @widen(5), b: @widen(5)) + b.x

  recur(Box(1), Box(2))