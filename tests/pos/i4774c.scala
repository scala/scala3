
import scala.quoted._

object Test {
  def loop[T](x: Expr[T])(implicit t: Type[T]): Staged[T] = '{ val y = $x; ${loop('y)} }
}
