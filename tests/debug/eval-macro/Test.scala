import Macro.showType

object Test:
  def main(args: Array[String]): Unit =
    val msg = "Hello, World!"
    println(showType(msg))
