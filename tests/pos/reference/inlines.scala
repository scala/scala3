package inlines

object Config {
  inline val logging = true
}

class Logger {
  protected[this] var a = 0
  protected [Logger] var b = 0
  protected var c = 0

}

object Logger {

  private var indent = 0
  private[this] var a = 0
  private[Logger] var b = 0

  inline def log[T](msg: String, indentMargin: => Int)(op: => T): T =
    if (Config.logging) {
      println(s"${"  " * indent}start $msg")
      indent += indentMargin
      val result = op
      indent -= indentMargin
      println(s"${"  " * indent}$msg = $result")
      result
    }
    else op
}

object Test{
  import Logger._

  var indentSetting = 2

  def factorial(n: BigInt): BigInt = {
    log(s"factorial($n)", indentSetting) {
      if (n == 0) 1
      else n * factorial(n - 1)
    }
  }

  def main(args: Array[String]): Unit =
    println(factorial(33))
}
