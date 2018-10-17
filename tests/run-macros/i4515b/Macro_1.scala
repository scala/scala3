import scala.quoted._

object Macro {
  inline def foo: Unit = ${ fooImpl }
  def fooImpl(implicit staging: StagingContext): quoted.Expr[Unit] = '{}
}
