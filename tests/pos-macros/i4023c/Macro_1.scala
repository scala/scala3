import scala.quoted.{_, given}
object Macro {
  inline def ff[T](x: T): T = ${ impl('x)('[T], summon[QuoteContext]) }
  def impl[T](x: Expr[T])(implicit t: Type[T], qctx: QuoteContext): Expr[T] = '{ $x: $t }
}
