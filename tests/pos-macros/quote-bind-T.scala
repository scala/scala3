
import scala.quoted._

object Test {
  def matchX[T](x: Expr[T])(using Staged[T], QuoteContext): Expr[T] = '{
    $x match {
      case y: T => y
    }
  }
}
