import scala.quoted.*
object Macro {
  inline def ff[T: Type](x: T): T = ${ impl('x) }
  def impl[T](x: Expr[T]): Expr[T] = x
}
