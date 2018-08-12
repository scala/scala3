import scala.quoted._

object Foo {
  rewrite def foo2(): Unit = ~foo2Impl()
  def foo2Impl(): Expr[Unit] = '()
  rewrite def foo(): Unit = foo2()
}
