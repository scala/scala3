import scala.quoted._

inline def defaultParams[T]: List[(String, Any)] = ${ defaultParamsImpl[T] }
private def defaultParamsImpl[T](
  using tpe: Type[T], qctx: QuoteContext): Expr[List[(String, Any)]] =
  import qctx.tasty._
  val sym = tpe.unseal.symbol
  val defaultParams = sym.defaultParams
  val values: List[Expr[Any]] =
    defaultParams.map { case (k, v) => Ref(v).seal }
  val names: List[Expr[String]] =
    defaultParams.map { case (k, v) => Expr(k) }
  '{ ${ Expr.ofList(names) }.zip(${ Expr.ofList(values) }) }
end defaultParamsImpl
