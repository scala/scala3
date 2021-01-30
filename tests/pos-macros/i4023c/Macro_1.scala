import scala.quoted.*
object Macro {
  inline def ff[T](x: T): T = ${ impl('x) }
  def impl[T](x: Expr[T])(implicit t: Type[T], qctx: Quotes): Expr[T] = '{ $x: T }
}
