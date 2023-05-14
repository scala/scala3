import scala.compiletime.*
import scala.quoted.*


object Logging {

  // Just use your imagination for now :)
  private inline val Trace = 0
  private inline val Debug = 1
  private inline val Info  = 2
  private inline val Warn  = 3

  private transparent inline def chosenThreshold: Int = ${
     choosenTresholdImpl
  }


  private def choosenTresholdImpl(using Quotes):Expr[Int] =
    import quotes.reflect.*
    MacroEnv.getInMacro("myLogger.level") match
        case Some("TRACE") => Expr(Trace)
        case Some("DEBUG") => Expr(Debug)
        case Some("INFO")  => Expr(Info)
        case Some("WARN")  => Expr(Warn)
        case Some(x)       => report.errorAndAbort("Unsupported logging level: " + x)
        case None          => Expr(Trace)

  private inline def log(inline lvl: Int, inline msg: String): Unit =
    inline if lvl >= chosenThreshold then println(msg)

  inline def trace(inline msg: String): Unit = log(Trace, msg)
  inline def debug(inline msg: String): Unit = log(Debug, msg)
  inline def info (inline msg: String): Unit = log(Info , msg)
  inline def warn (inline msg: String): Unit = log(Warn , msg)
}
