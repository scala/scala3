import scala.language.experimental.macros
import scala.reflect.macros._

object Macros {

  def foo0(x: Int): Int = macro foo0Impl // error
  def foo1(x: Int): Int = macro foo1Impl // error
  def foo2(x: Int, y: String): Int = macro foo2Impl // error
  def foo3(x: Int): Int = macro foo3Impl // error

  def foo0Impl: Nothing = ???
  def foo1Impl(context: scala.reflect.macros.Context)(x: context.Expr[String]): context.Expr[Int] = ???
  def foo2Impl(context: scala.reflect.macros.Context)(x: context.Expr[Int], y: context.Expr[String]): Nothing = ???
  def foo3Impl(x: Int): Int = ???

}
