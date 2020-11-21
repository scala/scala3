
import scala.quoted._

object Macro {
  def impl(opt: Expr[Option[Int]]) (using Quotes): Expr[Int] = opt.unliftOrError match {
    case Some(i) => Expr(i)
    case None => '{-1}
  }
}