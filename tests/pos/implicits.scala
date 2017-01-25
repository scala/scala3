object Test {

  class X(i: Int)

  implicit def int2x(i: Int): X = new X(i)

  val x: X = Byte.MinValue

  def foo() = {
    implicit val x = "abc"
    implicitly[String]
  }

}
