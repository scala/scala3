
import scala.quoted._

object Test {
  def loop[T](using s: Scope)(x: s.Expr[T])(implicit t: s.Type[T]): s.Expr[T] = '{
    val y: $t = $x
    ${loop('y)}
  }
}
