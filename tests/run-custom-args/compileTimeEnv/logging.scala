import scala.compiletime.*

object Logging {

  // Just use your imagination for now :)
  private inline val Trace = 0
  private inline val Debug = 1
  private inline val Info  = 2
  private inline val Warn  = 3

  private transparent inline def chosenThreshold: Int =
    inline envGet("myLogger.level") match
      case Some("TRACE") => Trace
      case Some("DEBUG") => Debug
      case Some("INFO")  => Info
      case Some("WARN")  => Warn
      case Some(x)       => error("Unsupported logging level: " + x)
      case None          => Trace

  private inline def log(inline lvl: Int, inline msg: String): Unit =
    inline if lvl >= chosenThreshold then println(msg) else ()

  inline def trace(inline msg: String) = log(Trace, msg)
  inline def debug(inline msg: String) = log(Debug, msg)
  inline def info (inline msg: String) = log(Info , msg)
  inline def warn (inline msg: String) = log(Warn , msg)
}

object Test {
  import Logging.*

  def main(args: Array[String]): Unit = {
    trace("I'm a trace msg")
    debug("I'm a debug msg")
    info("I'm a info msg")
    warn("I'm a warn msg")
  }
}
