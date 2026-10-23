object Sqrt {
  type NonNeg = {x: Int with x >= 0}
  
  def max(x: Int, y: Int): { v: Int with v >= x && v >= y } =
    if (x > y) x else y

  def sqrt(z: NonNeg): Double =
    scala.math.sqrt(z.toDouble)

  val u: Int = ???
  sqrt(max(0,u))
}
