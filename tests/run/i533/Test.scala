// scalajs: --skip

object Test {
  def main(args: Array[String]): Unit = {
    val x = new Array[Integer](1)
    x(0) = 10
    println(JA.get(x))
    println(JA.getVarargs(x*))
  }
}
