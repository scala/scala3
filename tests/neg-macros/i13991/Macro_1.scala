import scala.quoted.*

object InlineMac:

  inline def sample(inline expr: String): Int =
    ${ sampleImpl('expr) }

  def sampleImpl(expr: Expr[String])(using Quotes): Expr[Int] =
      import quotes.reflect.*
      report.errorAndAbort("Error", expr)
