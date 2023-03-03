trait Logger {
  inline def debug: debug = valueOf[debug]
  final type debug = false

  // fails
  inline final def log(inline s: String): Unit =
    inline if (debug) println(s)
}

trait BaseLogger extends Logger {
  // fails
  def bar() = log("case1")
}

object Logger {
  inline def log(s: String): Unit =
    inline if (valueOf[Logger#debug]) println(s)
}

class Test:
  def fails(x: BaseLogger) =
    x.log("case2")

  def works =
    Logger.log("case3")
