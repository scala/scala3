
import scala.quoted._

object Test {
  def matchX[T](x: Expr[T])(using Type[T], QuoteContext): Expr[T] = '{
    $x match {
      case y: T => y
    }
  }
}
