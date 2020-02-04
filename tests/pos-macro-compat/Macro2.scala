import scala.language.experimental.macros
import scala.reflect.macros._

object Macros {

  def foo1(x: Int): Int = macro foo1Impl
  def foo2(x: Int, y: String): Int = macro foo2Impl

  def foo1Impl(context: scala.reflect.macros.Context)(x: context.Expr[Int]): context.Expr[Int] = ???
  def foo2Impl(context: scala.reflect.macros.Context)(x: context.Expr[Int], y: context.Expr[String]): context.Expr[Int] = ???

}
