case class Box[Z](unbox: Z)

object Test {
  def foo(b: Box[Int |=> Int]): Int = b match {
    case Box(f) =>
      implicit val i: Int = 1
      f
  }
}
