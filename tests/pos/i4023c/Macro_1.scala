import scala.quoted._
object Macro {
  inline def ff[T](x: T): T = ${ impl('x)('[T]) }
  def impl[T](x: Expr[T])(implicit t: Type[T]): Expr[T] = '{ $x: $t }
}
