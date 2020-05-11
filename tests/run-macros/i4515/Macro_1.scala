import scala.quoted._
object Macro {
  inline def foo[X](x: X): Unit = ${fooImpl('x)}
  def fooImpl[X](using s: Scope)(x: s.Expr[X])(using s.Type[X]): s.Expr[Unit] = '{}
}
