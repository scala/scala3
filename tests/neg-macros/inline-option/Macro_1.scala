
import scala.quoted._

object Macro {
  def impl(opt: Expr[Option[Int]]) (using QuoteContext): Expr[Int] = opt.value match {
    case Some(i) => Lifted(i)
    case None => '{-1}
  }
}