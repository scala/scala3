
import scala.quoted._

object Macro {
  def impl(opt: Option[Int]) with QuoteContext : Expr[Int] = opt match {
    case Some(i) => Expr(i)
    case None => '{-1}
  }
}