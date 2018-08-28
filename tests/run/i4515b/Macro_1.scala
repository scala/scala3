import scala.tasty.Tasty

object Macro {
  rewrite def foo: Unit = ~fooImpl
  def fooImpl(implicit tasty: Tasty): quoted.Expr[Unit] = '()
}
