import scala.quoted._

object impl {
  def fooImpl(x: Expr[Int])(implicit qctx: QuoteContext): Expr[Int] = '{ val a = $x; a * a * a }
}