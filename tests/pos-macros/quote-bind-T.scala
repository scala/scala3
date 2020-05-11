
import scala.quoted._

object Test {
  def matchX[T](using s: Scope)(x: s.Expr[T])(using s.Type[T]): s.Expr[T] = '{
    $x match {
      case y: T => y
    }
  }
}
