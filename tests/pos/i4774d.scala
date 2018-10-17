
import scala.quoted._

object Test {
  def loop[T](x: Expr[T])(implicit t: Type[T]): Staged[T] =
    '{ val y: T = $x; ${loop('y)} }
}
