object Test {
  import scalatest._

  def main(args: Array[String]): Unit = {
    val l = List(3, 5)
    assert(l contains 3)
  }
}
