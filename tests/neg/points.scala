trait Comparable[T] {
  def compareTo(other: T): Int
}

class Point extends Comparable[Point] {
  override def compareTo(other: Point): Int = 1
}

class ColoredPoint extends Point with Comparable[ColoredPoint] { // error: cannot be instantiated
  override def compareTo(other: ColoredPoint): Int = -1
}

object Test extends App {
  val c: Point = new ColoredPoint
  def cmp[T <: Comparable[T]](p1: Comparable[T], p2: T) =
    p1.compareTo(p2)
  println(cmp(c, c))
}
