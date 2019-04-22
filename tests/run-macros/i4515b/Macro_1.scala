import scala.tasty.Reflection

object Macro {
  inline def foo: Unit = ${ fooImpl }
  def fooImpl(implicit reflect: Reflection): quoted.Expr[Unit] = '{}
}
