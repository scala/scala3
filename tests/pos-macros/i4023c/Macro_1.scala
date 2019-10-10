import scala.quoted._
object Macro {
  inline def ff[T](x: T): T = ${ impl('x)('[T], summon[QuoteContext]) }
  def impl[T](x: Expr[T])(implicit t: TypeTag[T], qctx: QuoteContext): Expr[T] = '{ $x: $t }
}
