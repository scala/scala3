import scala.quoted._

object Foo {
  transparent def foo2(): Unit = ~foo2Impl()
  def foo2Impl(): Expr[Unit] = '()
  transparent def foo(): Unit = foo2()
}
