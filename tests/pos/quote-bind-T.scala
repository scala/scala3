
import scala.quoted._

object Test {
  def matchX[T](x: Expr[T])(given TypeTag[T], QuoteContext): Expr[T] = '{
    $x match {
      case y: T => y
    }
  }
}
