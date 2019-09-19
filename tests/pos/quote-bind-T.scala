
import scala.quoted.{_, given}

object Test {
  def matchX[T](x: Expr[T])(given Type[T], QuoteContext): Expr[T] = '{
    $x match {
      case y: T => y
    }
  }
}
