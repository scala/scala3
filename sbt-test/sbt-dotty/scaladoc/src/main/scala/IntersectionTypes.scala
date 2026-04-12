package example

/**
  * Intersection Types: https://nightly.scala-lang.org/docs/reference/new-types/intersection-types.html
  */
object IntersectionTypes {

  sealed trait X {
    def x: Double
    def tpe: X
  }

  sealed trait Y {
    def y: Double
    def tpe: Y
  }

  type P = Y & X
  type PP = X & Y

  final case class Point(x: Double, y: Double) extends X with Y {
    override def tpe: X & Y = ???
  }

  def test: Unit = {

    def euclideanDistance(p1: X & Y, p2: X & Y) = {
      Math.sqrt(Math.pow(p2.y - p1.y, 2) + Math.pow(p2.x - p1.x, 2))
    }

    val p1: P = Point(3, 4)
    val p2: PP = Point(6, 8)
    println(euclideanDistance(p1, p2))

  }
}
