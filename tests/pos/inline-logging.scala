object Config {
  inline val logging = true
}

object Logger {

  private var indent = 0

  inline def log[T](msg: => String, indentMargin: Int)(op: => T): T =
    if (Config.logging) {
      val msgVal = msg
      println(s"${"  " * indent}start $msgVal")
      indent += indentMargin
      val result = op
      indent -= indentMargin
      println(s"${"  " * indent}$msgVal = $result")
      result
    }
    else op
}
import Logger._

var indentSetting = 2

def factorial(n: BigInt): BigInt = {
  log(s"factorial($n)", indentSetting) {
    if (n == 0) 1
    else n * factorial(n - 1)
  }
}
