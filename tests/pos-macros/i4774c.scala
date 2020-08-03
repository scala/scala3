
import scala.quoted._

object Test {
  def loop[T](x: Expr[T])(implicit t: Staged[T], qctx: QuoteContext): Expr[T] = '{ val y = $x; ${loop('y)} }
}
