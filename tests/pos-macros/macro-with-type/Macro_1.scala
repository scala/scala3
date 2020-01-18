import scala.quoted._
object Macro {
  inline def ff: Unit = ${impl('[Int])}
  def impl(t: Type[Int]) with QuoteContext : Expr[Unit] = '{}
}
