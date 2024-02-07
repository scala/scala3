// scalajs: --skip

object Test {
  def main(args: Array[String]): Unit = {
    val r1 = R1(42)
    r1 match {
      case R1(i) => assert(i == 42)
    }
  }
}
