object Test {
  import scalatest._

  case class Box[T](v: T) {
    def >(that: Box[T]): Boolean = this == that
  }

  trait EqInt
  implicit val eq: EqInt = new EqInt {}

  implicit class AnyOps[T](x: T) {
    def === (y: T)(implicit c: EqInt) = x == y
  }

  def main(args: Array[String]): Unit = {
    val a = Box(Some(10))
    val five: Float = 5.0f
    val six: Double = 6.0
    val ten: Int = 10
    assert(a.v === Some(10))
    assert(five < six)
    assert(five > 4)
    assert(ten > 5)
    assert(six < 7)
    assert(six > 5L)
    assert(Box(6) > Box(6))
    assert(Box("h") > Box("h"))
  }
}
