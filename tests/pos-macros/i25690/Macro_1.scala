import scala.quoted.*

object Tracer:
  trait Api:
    type Type

  val instance: Api = new Api { type Type = String }
  type Trace = instance.Type

  inline def autoTrace: Trace = ${ autoTraceImpl }

  def autoTraceImpl(using Quotes): Expr[Trace] =
    import quotes.reflect.*
    Typed(Literal(StringConstant("loc")), TypeTree.of[Trace]).asExprOf[Trace]
