object Test {
  import scalatest._

  implicit class AnyOps(x: String) extends AnyVal {
    def *(y: String): String = x + ", " + y
  }

  def main(args: Array[String]): Unit = {
    assert(("hello" * "world") == "hello, world")
  }
}
