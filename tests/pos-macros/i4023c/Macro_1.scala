import scala.quoted._
object Macro {
  inline def ff[T](x: T): T = ${ impl('x)('[T]) }
  def impl[T](using s: Scope)(x: s.Expr[T])(implicit t: s.Type[T]): s.Expr[T] = '{ $x: $t }
}
