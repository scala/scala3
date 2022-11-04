// scalajs: --skip

object Test:
  def main(args: Array[String]) =
    val a = JavaTest().run()
    println(a.mutableOneWithLongName)
