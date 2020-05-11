import scala.quoted._
object Macro {
  inline def ff[T](using s: Scope)(using t: s.Type[T]): Int = ${ impl[T] }
  def impl[T](using s: Scope): s.Expr[Int] = '{4}
}
