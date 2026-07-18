import scala.quoted.*

object Tracer:
  trait Api:
    type Type

  val instance: Api = new Api { type Type = String }
  type Trace = instance.Type

  inline def autoTrace: Trace = ${ autoTraceImpl }

  def autoTraceImpl(using Quotes): Expr[Trace] =
    import quotes.reflect.*
    // Upstream code changed in https://github.com/zio/zio/pull/10725
    '{"loc".asInstanceOf[Trace]}
