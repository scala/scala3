object Test {
  import scalatest.*

  def main(args: Array[String]): Unit = {
    assert(new Some(5).get == 5L)
  }
}
