import scala.quoted._
object Macro {
  inline def ff[T](using s: Scope)(x: T)(using s.Type[T]): T = ${ impl('x) }
  def impl[T](using s: Scope)(x: s.Expr[T]): s.Expr[T] = x
}
