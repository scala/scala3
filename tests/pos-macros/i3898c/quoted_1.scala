import scala.quoted.{_, given}
object Macro {
  inline def ff(x: Int, inline y: Int): String = ${impl('x)}
  def impl(x: Expr[Int])(given QuoteContext): Expr[String] = '{""}
}
