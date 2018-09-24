import scala.tasty.Tasty

object Macro {
  inline def foo: Unit = ~fooImpl
  def fooImpl(implicit tasty: Tasty): quoted.Expr[Unit] = '()
}
