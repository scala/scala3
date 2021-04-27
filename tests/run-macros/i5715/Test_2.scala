object Test {
  import scalatest.*

  def main(args: Array[String]): Unit = {
    val l = List(3, 4)
    assert(l.exists(_ == 3))
  }
}
