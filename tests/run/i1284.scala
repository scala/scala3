case object A
case object B

object Test {
  def main(args: Array[String]): Unit = {
    assert(Array(A, B).toList.toString == "List(A, B)")
  }
}
