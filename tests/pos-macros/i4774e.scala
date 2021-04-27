
import scala.quoted.*

object Test {
  def loop[T](x: Expr[T])(implicit t: Type[T], qctx: Quotes): Expr[T] =
    '{ def y = $x; ${ loop('y) } }

  def loop2[T](x: Expr[T])(implicit t: Type[T], qctx: Quotes): Expr[T] =
    '{ def y() = $x; ${ loop('{y()}) } }
}
