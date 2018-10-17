import scala.quoted._

object Macros {
  def printStack(tag: String): Unit = {
    println(tag + ": "+ new Exception().getStackTrace().apply(1))
  }
  def assertImpl(expr: Expr[Boolean]): Staged[Unit] = '{
    printStack("assertImpl")
    println($expr)
  }
}
