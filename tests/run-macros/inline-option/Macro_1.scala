
import scala.quoted._
import scala.quoted.autolift.{given _}

object Macros {

  def impl(opt: Expr[Option[Int]]) (using QuoteContext): Expr[Int] = opt.value match {
    case Some(i) => i
    case None => '{-1}
  }

  def impl2(opt: Expr[Option[Option[Int]]]) (using QuoteContext): Expr[Int] = impl(opt.value.flatten)

}
