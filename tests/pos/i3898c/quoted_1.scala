import scala.quoted._
object Macro {
  rewrite def ff(x: Int, transparent y: Int): String = ~impl('(x))
  def impl(x: Expr[Int]): Expr[String] = '("")
}
