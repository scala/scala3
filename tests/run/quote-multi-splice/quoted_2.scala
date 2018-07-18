object Test {
  import PowerMacro._

  def main(args: Array[String]): Unit = {
    println(powerV1(1, 5.0))
    println(powerV1(-1, 5.0))
    println(powerV2(1, 5.0))
    println(powerV2(-1, 5.0))
  }
}
