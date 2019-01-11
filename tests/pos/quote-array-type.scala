import scala.quoted._

object Arrays {
  def toExpr[T](implicit t: Type[T]): Expr[Array[List[T]]] = '{
    new Array[List[~t]](0)
  }
}
