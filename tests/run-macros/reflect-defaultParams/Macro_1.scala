import scala.quoted._

inline def defaultParams[T]: Map[String, Any] = ${ defaultParamsImpl[T] }
private def defaultParamsImpl[T](
  using tpe: Type[T], qctx: QuoteContext): Expr[Map[String, Any]] =
  import qctx.tasty._
  val sym = tpe.unseal.symbol
  val exprs: Map[String, Expr[Any]] =
    sym.defaultParams.view.mapValues(_.seal.cast[Any]).toMap
  Expr.ofMapValues(exprs)
end defaultParamsImpl
