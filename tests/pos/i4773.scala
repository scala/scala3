import scala.quoted._

object Foo {
  inline def foo2(): Unit = ${foo2Impl()}
  def foo2Impl(): Expr[Unit] = '{}
  inline def foo(): Unit = foo2()
}
