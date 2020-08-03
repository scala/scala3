import scala.quoted._
object Macro {
  inline def ff[T](x: T): T = ${ impl('x)('[T], summon[QuoteContext]) }
  def impl[T](x: Expr[T])(implicit t: Staged[T], qctx: QuoteContext): Expr[T] = '{ $x: $t }
}
