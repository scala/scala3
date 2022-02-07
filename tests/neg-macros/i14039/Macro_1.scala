import scala.quoted.*

object Macro:
  inline def apply(): Any = ${Macro.impl}

  def impl(using Quotes): Expr[Any] =
    quotes.reflect.report.errorAndAbort("my message")
