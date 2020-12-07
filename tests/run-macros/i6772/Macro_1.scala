import scala.quoted._

object Macros {

  inline def m() : Any = ${  mImpl() }

  def mImpl()(using Quotes): Expr[Any] =
    List(Value(1), Value(2), Value(3)).toExprOfList

  extension [T](list: List[Expr[T]]) def toExprOfList(using Type[T], Quotes): Expr[List[T]] = '{
    val buff = List.newBuilder[T]
    ${ Expr.block(list.map(v => '{ buff +=  $v }), '{ buff.result() }) }
  }
}
