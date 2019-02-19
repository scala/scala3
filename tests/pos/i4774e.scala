
import scala.quoted._

object Test {
  def loop[T](x: Expr[T])(implicit t: Type[T]): Expr[T] =
    '{ def y = $x; ${ loop('y) } }

  def loop2[T](x: Expr[T])(implicit t: Type[T]): Expr[T] =
  '{ def y() = $x; ${ loop('{y()}) } }
}
