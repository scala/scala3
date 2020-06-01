import scala.quoted._

object Macros {

  inline def m() : Any = ${  mImpl() }

  def mImpl()(using QuoteContext): Expr[Any] =
    List(Expr(1), Expr(2), Expr(3)).toExprOfList

  def [T](list: List[Expr[T]]).toExprOfList(using Type[T], QuoteContext): Expr[List[T]] = '{
    val buff = List.newBuilder[T]
    ${ Expr.block(list.map(v => '{ buff +=  $v }), '{ buff.result() }) }
  }
}
