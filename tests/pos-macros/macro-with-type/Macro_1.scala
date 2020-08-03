import scala.quoted._
object Macro {
  inline def ff: Unit = ${impl('[Int])}
  def impl(t: Staged[Int])(using QuoteContext): Expr[Unit] = '{}
}
