import scala.quoted._
object Macro {
  inline def ff(x: Int, inline y: Int): String = ${impl('x)}
  def impl(x: Expr[Int]): Expr[String] = '{""}
}
