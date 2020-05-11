import scala.quoted._

object Macros {

  inline def m() : Any = ${  mImpl() }

  def mImpl()(using s: Scope): s.Expr[Any] =
    toExprOfList(List(Expr(1), Expr(2), Expr(3)))

  def toExprOfList[T](using s: Scope)(list: List[s.Expr[T]])(using s.Type[T]): s.Expr[List[T]] = '{
    val buff = List.newBuilder[T]
    ${ Expr.block(list.map(v => '{ buff +=  $v }), '{ buff.result() }) }
  }
}
