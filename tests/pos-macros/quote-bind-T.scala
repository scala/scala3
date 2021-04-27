
import scala.quoted.*

object Test {
  def matchX[T](x: Expr[T])(using Type[T], Quotes): Expr[T] = '{
    $x match {
      case y: T => y
    }
  }
}
