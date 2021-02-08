import scala.quoted.*

object Foo {
  inline def foo2(): Unit = ${foo2Impl()}
  def foo2Impl()(using Quotes): Expr[Unit] = '{}
  inline def foo(): Unit = foo2()
}
