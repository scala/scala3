import scala.quoted.*

object Macros {
  inline def buildString = ${buildStringCode}

  def buildStringCode(using Quotes): Expr[String] = {
    import quotes.reflect.*
    val str: String = null
    try
      Expr(str)
    catch
      case error: java.lang.IllegalArgumentException =>
        error.setStackTrace(Array[StackTraceElement]())
        throw error
  }
}
