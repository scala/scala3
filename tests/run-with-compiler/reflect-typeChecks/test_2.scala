object Test {
  import scalatest._

  trait Eq[T]
  implicit val eq: Eq[Int] = new Eq[Int] {}

  implicit class AnyOps[T](x: T) {
    def === (y: T)(implicit c: Eq[T]) = x == y
  }

  def main(args: Array[String]): Unit = {
    assertCompiles("5 === 5")
    assertNotCompile("5.6 === 7.7")
  }
}
