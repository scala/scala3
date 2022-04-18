object Test {
  val a: Array[Long] = Array(1L)

  def test(x: Any) = x match {
    case Array(i: Long) => println("Success!")
    case _              => println("Failure!") }

  def main(args: Array[String]): Unit = test(a)
}