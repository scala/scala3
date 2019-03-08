
import scala.quoted._

object Test {
  def loop[T](x: Expr[T])(implicit t: Type[T]): Expr[T] = '{
    val y: $t = $x
    ${loop('y)}
  }
}
