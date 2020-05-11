
import scala.quoted._

object Test {
  def loop[T](using s: Scope)(x: s.Expr[T])(using s.Type[T]): s.Expr[T] = '{ val y = $x; ${loop('y)} }
}
