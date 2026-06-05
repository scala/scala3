import scala.quoted.*

object Tracer:
  trait Api:
    type Type

  val instance: Api = new Api { type Type = String }
  type Trace = instance.Type

  inline def autoTrace: Trace = ${ autoTraceImpl }

  def autoTraceImpl(using Quotes): Expr[Trace] =
    import quotes.reflect.*
    Literal(StringConstant("loc")).asExprOf[String].asInstanceOf[Expr[Trace]]
