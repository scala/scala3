
object Test {
  def main(args: Array[String]): Unit = {
    0 match
      case Succ(n) => ???
      case _ =>

    2 match
      case Succ(n) => assert(n == 1)
  }

}
