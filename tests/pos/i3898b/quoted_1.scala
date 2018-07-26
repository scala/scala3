import scala.quoted._
object Macro {
  transparent def ff(x: Int, transparent y: Int): String = ~impl('(x))
  def impl(x: Expr[Int]): Expr[String] = '("")
}
