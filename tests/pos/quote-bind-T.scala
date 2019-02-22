
import scala.quoted._

object Test {
  def matchX[T](x: Expr[T])(implicit t: Type[T]): Expr[T] = '{
    $x match {
      case y: T => y
    }
  }
}
