import scala.quoted.*

object Macros {

  inline def m() : Any = ${  mImpl() }

  def mImpl()(using Quotes): Expr[Any] =
    List(Expr(1), Expr(2), Expr(3)).toExprOfList

  extension [T](list: List[Expr[T]]) def toExprOfList(using Type[T], Quotes): Expr[List[T]] = '{
    val buff = List.newBuilder[T]
    ${ Expr.block(list.map(v => '{ buff +=  $v }), '{ buff.result() }) }
  }
}
