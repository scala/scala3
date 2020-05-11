import scala.quoted._

object Macros {
  def printStack(tag: String): Unit = {
    println(tag + ": "+ new Exception().getStackTrace().apply(1))
  }
  def assertImpl(using Scope)(expr: scope.Expr[Boolean]) = '{
    printStack("assertImpl")
    println($expr)
  }
}
