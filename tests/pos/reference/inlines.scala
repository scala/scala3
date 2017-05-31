package inlines

object Config {
  inline val logging = false
}

object Logger {

  private var indent = 0

  inline def log[T](msg: String)(op: => T): T =
    if (Config.logging) {
      println(s"${"  " * indent}start $msg")
      indent += 1
      val result = op
      indent -= 1
      println(s"${"  " * indent}$msg = $result")
      result
    }
    else op
}

object Test {
  import Logger._

  def factorial(n: BigInt): BigInt =
    log(s"factorial($n)") {
      if (n == 0) 1
      else n * factorial(n - 1)
    }

  def main(args: Array[String]): Unit =
    println(factorial(33))
}
