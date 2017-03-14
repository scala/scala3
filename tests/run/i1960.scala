case class CC2[A, B](_1: A, _2: B)

object Test {
  def main(args: Array[String]): Unit = {
    val CC2(_, CC2(a, _)) = CC2(0, CC2(1, 2))
    assert(a == 1)
  }
}
