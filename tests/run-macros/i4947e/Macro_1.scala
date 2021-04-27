import scala.quoted.*

object Macros {
  def printStack(tag: String): Unit = {
    println(tag + ": "+ new Exception().getStackTrace().apply(1))
  }
  def assertImpl(expr: Expr[Boolean])(using Quotes) = '{
    printStack("assertImpl")
    println($expr)
  }
}
