
import scala.quoted._

object Macro {
  def impl(opt: Expr[Option[Int]]) (using Quotes): Expr[Int] = opt.valueOrError match {
    case Some(i) => Value(i)
    case None => '{-1}
  }
}