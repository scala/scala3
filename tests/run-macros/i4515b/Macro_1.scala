import scala.quoted._

object Macro {
  inline def foo: Unit = ${ fooImpl }
  def fooImpl(using s: Scope): s.Expr[Unit] = '{}
}
