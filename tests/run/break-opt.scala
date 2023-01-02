import scala.util.*

object breakOpt:

  var zero = 0

  def test1(x: Int): Int =
    boundary:
      if x < 0 then break(zero)
      x

  def test2(xx: Int, xs: List[Int]): Int =
    boundary:
      if xx < 0 then break(zero)
      xs.map: y =>
        if y < 0 then break(zero)
        y
      xx + xs.sum

  def test3(xx: Int, xs: List[Int]): Int =
    def cond[T](p: Boolean, x: => T, y: => T): T =
      if p then x else y
    boundary:
      cond(true, { if xx < 0 then break(zero); xx }, xx)

  def test3a(xx: Int, xs: List[Int]): Int =
    inline def cond[T](p: Boolean, inline x: T, y: => T): T =
      if p then x else y
    boundary:
      cond(true, { if xx < 0 then break(zero); xx }, xx)

  def test4(x: Int): Int =
    boundary:
      try
        if x < 0 then break(zero)
        boundary:
          if x == 0 then break(-1)
          x
      finally
        println("done")

  def test5(x: Int): Int =
    boundary: lab1 ?=>
      if x < 0 then break(zero)
      boundary:
        if x == 0 then break(-1)
        if x > 0 then break(+1)(using lab1)
        x

  def test6(x0: Int): Int =
    var x = x0
    var y = x
    boundary:
      while true do
        y = y * x
        x -= 1
        if x == 0 then break()
    y

@main def Test =
  import breakOpt.*
  assert(test1(0) == 0)
  assert(test1(-1) == 0)
  assert(test2(1, List(1, 2, 3)) == 7)
  assert(test2(-1, List(1, 2, 3)) == 0)
  assert(test2(1, List(1, -2, 3)) == 0)
  test4(1)
  test4(-1)
  assert(test5(2) == 1)
  assert(test6(3) == 18)


