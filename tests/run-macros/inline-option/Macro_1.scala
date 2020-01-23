
import scala.quoted._
import scala.quoted.autolift.given

object Macros {

  def impl(opt: Option[Int]) with QuoteContext : Expr[Int] = opt match {
    case Some(i) => i
    case None => '{-1}
  }

  def impl2(opt: Option[Option[Int]]) with QuoteContext : Expr[Int] = impl(opt.flatten)

}
