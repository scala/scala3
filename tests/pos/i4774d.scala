
import scala.quoted._

object Test {
  def loop[T](x: Expr[T])(implicit t: TypeTag[T], qctx: QuoteContext): Expr[T] =
    '{ val y: T = $x; ${loop('y)} }
}
