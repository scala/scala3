import scala.quoted._
object Macro {
  inline def ff[T](x: T): T = ${ impl('x)(Type[T], summon[QuoteContext]) }
  def impl[T](x: Expr[T])(implicit t: Type[T], qctx: QuoteContext): Expr[T] = '{ $x: $t }
}
