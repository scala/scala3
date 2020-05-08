import scala.language.experimental.macros

class Context: // Dummy scala.reflect.macros.Context
  type Expr[T]

object Macros {

  def foo1(x: Int): Int = macro foo1Impl
  def foo1(x: Int): Int = ???

  def foo2(x: Int, y: String): Int = macro foo2Impl
  def foo2(x: Int, y: String): Int = ???

  def foo1Impl(context: Context)(x: context.Expr[Int]): context.Expr[Int] = ???
  def foo2Impl(context: Context)(x: context.Expr[Int], y: context.Expr[String]): context.Expr[Int] = ???

}
