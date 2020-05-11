import scala.quoted._
object Foo {
  inline def foo[X](x: X): Unit = ${fooImpl('x)}
  def fooImpl[X](using s: Scope)(x: X)(using s.Type[X]): s.Expr[Unit] = '{}
}
