
import scala.quoted._

object Test {
  def loop[T](using s: Scope)(x: s.Expr[T])(implicit t: s.Type[T]): s.Expr[T] =
    '{ def y = $x; ${ loop('y) } }

  def loop2[T](using s: Scope)(x: s.Expr[T])(implicit t: s.Type[T]): s.Expr[T] =
    '{ def y() = $x; ${ loop('{y()}) } }
}
