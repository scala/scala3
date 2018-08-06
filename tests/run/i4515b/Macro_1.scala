import scala.tasty.Tasty

object Macro {
  transparent def foo: Unit = ~fooImpl
  def fooImpl(implicit tasty: Tasty): quoted.Expr[Unit] = '()
}
