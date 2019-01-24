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
    assert(a.v === Some(10))
    assert(10 > 5)
    assert(4.0 < 5)
    assert(6.0 > 5L)
    assert(Box(6) > Box(6))
    assert(Box("h") > Box("h"))
  }
}
