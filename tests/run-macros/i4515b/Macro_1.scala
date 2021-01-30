import scala.quoted.*

object Macro {
  inline def foo: Unit = ${ fooImpl }
  def fooImpl(using Quotes): Expr[Unit] = '{}
}
