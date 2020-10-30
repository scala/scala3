
import scala.quoted._

object Test {
  def loop[T](x: Expr[T])(implicit t: Type[T], qctx: QuoteContext): Expr[T] = '{
    val y: t.Underlying = $x;
    ${loop[t.Underlying](
      'y
    )}
  }
}
