import scala.quoted._
object Macro {
  transparent def ff(x: Int, y: Int & Constant): String = ~impl('(x))
  def impl(x: Expr[Int]): Expr[String] = '("")
}
